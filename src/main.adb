with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ada.Sequential_IO;

with Key; use Key;
with Permutation;
with Feistel;

procedure Main is 
    Max_Length: constant Integer := 8;
    package Byte_IO is new Ada.Sequential_IO (Unsigned_8);
    package IO_64 is new Ada.Sequential_IO (Unsigned_64);
    type Byte_Array is array(1 .. Max_Length) of Unsigned_8;

    Filename:  String(1 .. 256);
    Length: Integer;
    
    F: Byte_IO.File_Type;
    OutFile: IO_64.File_Type;

    BlockArr: Byte_Array;
    Byte: Unsigned_8;
    Block64: Unsigned_64;
    BytePos: Integer range 1 .. 8;

    Key: Unsigned_64 := 16#22234512987ABB23#;
    Keys: Keys_Array;

    -- Adds padding to block
    function InsertPadding(Block: Unsigned_64) return Unsigned_64 is
    begin
        -- TODO
        return 0;
    end InsertPadding;

    -- Given an array of 8 bytes, returns a 64 bit integer  
    function AssembleBlock(BlockArr: Byte_Array) return Unsigned_64 is
    Block: Unsigned_64 := 0;  
    begin
        for I in 1 .. 8 loop
            Block := Block or shift_left(Unsigned_64(BlockArr(I)), (8-I)*8); 
        end loop;
        return Block;
    end AssembleBlock;     

    function DES_Function(R: Unsigned_32; K: Unsigned_64) return Unsigned_32 is
    Expanded: Unsigned_64;
    Result  : Unsigned_32;  
    begin
        -- Expands R to 48 bits

        Expanded := Feistel.Expansion(R);

        -- R XOR K

        Expanded := Expanded or K;

        -- S-Boxes
        
        Result := Feistel.S_Boxes(Expanded);

        -- Straight

        Result := Feistel.Straight (Result);

        return Result;
    end DES_Function;

    -- Performs 16 feistel rounds
    function Rounds(Block: Unsigned_64) return Unsigned_64 is
    L0: Unsigned_32 := 0; 
    R0: Unsigned_32 := 0; 
    Result: Unsigned_64;  
    begin
        L0 := Unsigned_32 (shift_right(Block, 32));
        R0 := Unsigned_32 (Block and 16#FFFFFFFF#);

        for Round in 1 .. 16 loop
            -- put_line("in development");
            R0 := DES_Function(R0, Keys(Round));
        end loop;

        Result := shift_left(Unsigned_64 (R0), 32) or Unsigned_64 (L0);

        return Result;
    end Rounds; 

begin
    put_line ("Plaintext filename: ");
    get_line (Filename, Length);
    Byte_IO.open (F, Byte_IO.In_File, Filename);

    Keys := KeysGen(Key);

    BytePos := 1;
    while not Byte_IO.End_Of_File (F) loop
        Byte_IO.read(F, Byte);
        BlockArr(BytePos) := Byte;

        if BytePos = 8 then
            Block64 := AssembleBlock(BlockArr);
            --put_line(Unsigned_64'Image (Block64));
            -- initial perm
            Block64 := Permutation.IP(Block64);
            --put_line(Unsigned_64'Image (Permutation.IP(Block64)));
            -- rounds
            Block64 := Rounds(Block64);
            put_line(Unsigned_64'Image (Block64));

            -- Write output
            IO_64.Create(OutFile, IO_64.Out_File, "Output");
            IO_64.Write (OutFile, Block64);
            IO_64.Close (OutFile);

        end if;
        BytePos := (BytePos mod 8) + 1;
    
    end loop;

    Byte_IO.Close (F);

    -- TODO add padding if not multiple of 8

    -- put_line("Cyphertext:");
   
end Main;