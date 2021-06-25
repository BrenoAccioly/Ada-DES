with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ada.Sequential_IO;

with Permutation;


procedure Main is 
    Max_Length: constant Integer := 8;
    package Byte_IO is new Ada.Sequential_IO (Unsigned_8);
    type Byte_Array is array(1 .. Max_Length) of Unsigned_8;

    Filename:  String(1 .. 256);
    Length: Integer;
    
    F: Byte_IO.File_Type;
    BlockArr: Byte_Array;
    Byte: Unsigned_8;
    Block64: Unsigned_64;
    BytePos: Integer range 1 .. 8;

    -- Adds padding to block
    function InsertPadding(Block: Unsigned_64) return Unsigned_64 is
    begin
        return 0;
    end InsertPadding;

    function AssembleBlock(BlockArr: Byte_Array) return Unsigned_64 is
    Block: Unsigned_64 := 0;  
    begin
        for I in 1 .. 8 loop
            Block := Block or shift_left(Unsigned_64(BlockArr(I)), (8-I)*8); 
        end loop;
        return Block;
    end AssembleBlock;    

begin
    put_line ("Plaintext filename: ");
    get_line (Filename, Length);
    Byte_IO.open (F, Byte_IO.In_File, Filename);

    BytePos := 1;
    while not Byte_IO.End_Of_File (F) loop
        Byte_IO.read(F, Byte);
        BlockArr(BytePos) := Byte;

        if BytePos = 8 then
            Block64 := AssembleBlock(BlockArr);
            put_line(Unsigned_64'Image (Block64));
            put_line(Unsigned_64'Image (Permutation.IP(Block64)));
        end if;
        BytePos := (BytePos mod 8) + 1;
    
    end loop;
    --put_line("Cyphertext:");
   
end Main;