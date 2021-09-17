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
    package U64_IO is new Ada.Text_IO.Modular_IO (Interfaces.unsigned_64);
    type Byte_Array is array(1 .. Max_Length) of Unsigned_8;

    Filename:  String(1 .. 256);
    Length: Integer;
    
    F: Byte_IO.File_Type;
    OutFile: IO_64.File_Type;

    BlockArr: Byte_Array;
    Byte: Unsigned_8;
    Block64: Unsigned_64;
    BytePos: Integer range 1 .. 8;

    Key: Unsigned_64;                
    Keys: Keys_Array;

    -- Adds padding to block (PKCS#7)
    function InsertPaddingCMS(BlockArr: Byte_Array; Bytes: Natural) return Byte_Array is
    NewBlock: Byte_Array;
    begin
        NewBlock := BlockArr;
        for I in 1 .. Bytes loop
            NewBlock (9-I) := Unsigned_8 (Bytes);
        end loop;
        return NewBlock;
    end InsertPaddingCMS;   

    function DES_Function(R: Unsigned_32; K: Unsigned_64) return Unsigned_32 is
    Expanded: Unsigned_64;
    Result  : Unsigned_32;  
    begin
        -- Expands R to 48 bits
        Expanded := Feistel.Expansion(R);
        -- R XOR K
        Expanded := Expanded xor K;
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
    Temp: Unsigned_32 := 0;
    Result: Unsigned_64 := 0;  
    begin
        L0 := Unsigned_32 (shift_right(Block, 32));
        R0 := Unsigned_32 (Block and 16#FFFFFFFF#);

        for Round in 1 .. 16 loop
            Temp := R0;
            R0 := L0 xor DES_Function(R0, Keys(Round));
            L0 := Temp;
        end loop;

        Result := shift_left(Unsigned_64 (R0), 32) or Unsigned_64 (L0);

        return Result;
    end Rounds;

    -- Encrypts block
    function AssembleBlock(BlockArr: Byte_Array) return Unsigned_64 is
    Block: Unsigned_64 := 0;  
    begin
        for I in 1 .. 8 loop
            Block := Block or shift_left(Unsigned_64(BlockArr(I)), (I-1)*8); 
        end loop;

        -- initial perm
        Block := Permutation.IP(Block);

        -- rounds
        Block := Rounds(Block);

        -- Final Permutation
        Block := Permutation.LP (Block);

        return Block;
    end AssembleBlock;  

begin
    put_line ("Plaintext filename:");
    get_line (Filename, Length);

    Put_Line ("Key (In Hex):");
    begin
        Key := Unsigned_64'Value ("16#" & Get_Line & "#");
    exception
        when Constraint_Error => 
            Put_Line ("Keys must be 64 bits long, inserted in hexadecimal");
            return;
    end;

    --key := 16#AABB09182736CCDD#;

    U64_IO.Put(
        Key,
        Base=>16
    );
    New_Line;
    Byte_IO.open (F, Byte_IO.In_File, Filename);
    IO_64.Create(OutFile, IO_64.Out_File, "Output");
    
    Keys := KeysGen(Key, True);
    --Keys := KeysGen(Key, False);
    BytePos := 1;
    while not Byte_IO.End_Of_File (F) loop
        Byte_IO.read(F, Byte);
        BlockArr(BytePos) := Byte;

        if BytePos = 8 then
            Block64 := AssembleBlock(BlockArr);
            --U64_IO.Put(
            --    Block64,
            --    Base=>16
            --);
            --New_Line;
            -- Write output
            IO_64.Write (OutFile, Block64);
        end if;
        BytePos := (BytePos mod 8) + 1;
    end loop;
    Byte_IO.Close (F);

    if (BytePos /= 1) then
        -- Insert Padding
        BlockArr := InsertPaddingCMS (BlockArr, 9-BytePos);
        Block64 := AssembleBlock(BlockArr);
        IO_64.Write (OutFile, Block64);
    end if;
    IO_64.Close (OutFile);
   
end Main;