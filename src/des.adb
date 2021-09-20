package body DES is

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
    function Rounds(Block: Unsigned_64; Keys: Keys_Array) return Unsigned_64 is
    L:     Unsigned_32 := 0; 
    R:     Unsigned_32 := 0; 
    Temp:   Unsigned_32 := 0;
    Result: Unsigned_64 := 0;  
    begin
        L := Unsigned_32 (shift_right(Block, 32));
        R := Unsigned_32 (Block and 16#FFFFFFFF#);

        for Round in 1 .. 16 loop
            Temp := R;
            R := L xor DES_Function(R, Keys(Round));
            L := Temp;
        end loop;

        Result := Shift_Left(Unsigned_64 (R), 32) or Unsigned_64 (L);
        return Result;
    end Rounds;

    -- Encrypts and assembles block
    function AssembleBlock(BlockArr: Byte_Array; Keys: Keys_Array) return Unsigned_64 is
    Block: Unsigned_64 := 0;
    FinalBlock: Unsigned_64 := 0;
    begin
        for I in 1 .. 8 loop
            Block := Block or Shift_Left(Unsigned_64(BlockArr(I)), (8-I)*8); 
        end loop;
        -- initial perm
        Block := Permutation.IP(Block);
        -- rounds
        Block := Rounds (Block, Keys);
        -- Final Permutation
        Block := Permutation.LP (Block);
        for I in 1 .. 8 loop
            FinalBlock := FinalBlock or Shift_Left(Shift_Right(Block, (I-1)*8) and 16#FF#, (8-I)*8); 
        end loop;
        return FinalBlock;
    end AssembleBlock;

    procedure EncryptFile(Encrypt: Boolean; Key: Unsigned_64; InFileName: String; OutFileName: String) is
    InFile  : Byte_IO.File_Type;
    OutFile : IO_64.File_Type;
    BlockArr: Byte_Array;
    Byte    : Unsigned_8;
    Block64 : Unsigned_64;
    BytePos : Integer range 1 .. 8;         
    Keys    : Keys_Array;
    begin
        Byte_IO.open (InFile, Byte_IO.In_File, InFileName);
        
        IO_64.Create(OutFile, IO_64.Out_File, OutFileName);
        Keys := KeysGen(Key, Encrypt);

        BytePos := 1;
        while not Byte_IO.End_Of_File (InFile) loop
            Byte_IO.read(InFile, Byte);
            BlockArr(BytePos) := Byte;
            if BytePos = 8 then
                Block64 := AssembleBlock(BlockArr, Keys);
                --U64_IO.Put(Block64, Base=>16);New_Line;
                IO_64.Write (OutFile, Block64);
            end if;
            BytePos := (BytePos mod 8) + 1;
        end loop;
        Byte_IO.Close (InFile);

        if (BytePos /= 1) then
            -- Insert Padding
            BlockArr := InsertPaddingCMS (BlockArr, 9-BytePos);
            Block64 := AssembleBlock(BlockArr, Keys);
            IO_64.Write (OutFile, Block64);
        end if;
        IO_64.Close (OutFile);

        if Encrypt then
            Put_Line ("Encryption complete");
        else
            Put_Line ("Decryption complete");
        end if;

    end EncryptFile;

end DES;