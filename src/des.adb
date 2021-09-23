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

    function AssembleBlockToEncrypt (BlockArr: Byte_Array) return Unsigned_64 is
    Block: Unsigned_64 := 0;
    begin
        for I in 1 .. 8 loop
            Block := Block or Shift_Left(Unsigned_64(BlockArr(I)), (8-I)*8); 
        end loop;
        return Block;
    end AssembleBlockToEncrypt;

    function AssembleBlockToWrite (Block: Unsigned_64) return Unsigned_64 is
    NewBlock: Unsigned_64 := 0;
    begin
        for I in 1 .. 8 loop
            NewBlock := NewBlock or Shift_Left(Shift_Right(Block, (I-1)*8) and 16#FF#, (8-I)*8); 
        end loop;
        return NewBlock;
    end AssembleBlockToWrite;

    function EncryptBlock (Block: Unsigned_64; Keys: Keys_Array) return Unsigned_64 is
    NewBlock: Unsigned_64 := 0;
    begin
        -- initial perm
        NewBlock := Permutation.Permute (Block, Permutation.IP_Arr);
        -- rounds
        NewBlock := Rounds (NewBlock, Keys);
        -- Final Permutation
        NewBlock := Permutation.Permute (NewBlock, Permutation.LP_Arr);
        return NewBlock;
    end EncryptBlock;

    procedure EncryptFile(Encrypt: Boolean; Key: Unsigned_64; InFileName: String; OutFileName: String; ModeOfOperation: Natural; IV: Unsigned_64) is
    InFile      : Byte_IO.File_Type;
    OutFile     : IO_64.File_Type;
    BlockArr    : Byte_Array;
    Byte        : Unsigned_8;
    BytePos     : Integer range 1 .. 8;
    Block64     : Unsigned_64;
    LastBlock   : Unsigned_64 := IV;
    LastBlockAux: Unsigned_64;         
    Keys        : Keys_Array;
    begin
        Byte_IO.open (InFile, Byte_IO.In_File, InFileName);
        IO_64.Create(OutFile, IO_64.Out_File, OutFileName);
        Keys    := KeysGen(Key, Encrypt);
        BytePos := 1;
        while not Byte_IO.End_Of_File (InFile) loop
            Byte_IO.read(InFile, Byte);
            BlockArr(BytePos) := Byte;
            if BytePos = 8 then
                Block64   := AssembleBlockToEncrypt(BlockArr);
                
                if ModeOfOperation=DES.CBC_MODE then
                    if Encrypt then
                        Block64 := Block64 xor LastBlock;
                    else
                        LastBlockAux := Block64;   
                    end if;
                end if;
                
                Block64   := EncryptBlock(Block64, Keys);
                             
                if ModeOfOperation=DES.CBC_MODE then
                    if Encrypt then
                        LastBlock := Block64;  
                    else
                        Block64 := Block64 xor LastBlock;
                        LastBlock := LastBlockAux;
                    end if;
                end if;

                Block64   := AssembleBlockToWrite (Block64);

                IO_64.Write (OutFile, Block64);
            end if;
            BytePos := (BytePos mod 8) + 1;
        end loop;
        Byte_IO.Close (InFile);

        if (BytePos /= 1) then
            -- Insert Padding
            BlockArr := InsertPaddingCMS (BlockArr, 9-BytePos);
            Block64   := AssembleBlockToEncrypt(BlockArr);
            
            if ModeOfOperation=DES.CBC_MODE then
                if Encrypt then
                    Block64 := Block64 xor LastBlock;
                else
                    LastBlock := Block64;   
                end if;
            end if;
            
            Block64   := EncryptBlock(Block64, Keys);
                         
            if ModeOfOperation=DES.CBC_MODE then
                if Encrypt then
                    LastBlock := Block64;  
                else
                    Block64 := Block64 xor LastBlock;
                end if;
            end if;

            Block64   := AssembleBlockToWrite (Block64);
            IO_64.Write (OutFile, Block64);
        end if;
        IO_64.Close (OutFile);
    end EncryptFile;

end DES;