with GNAT.Command_Line;   use GNAT.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ada.Sequential_IO;
with GNAT.Strings; use GNAT.Strings;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed;

with Key; use Key;
with Permutation;
with Feistel;

procedure Main is 
    Max_Length: constant Integer := 8;
    package Byte_IO is new Ada.Sequential_IO (Unsigned_8);
    package IO_64 is new Ada.Sequential_IO (Unsigned_64);
    package U64_IO is new Ada.Text_IO.Modular_IO (Interfaces.unsigned_64);
    type Byte_Array is array(1 .. Max_Length) of Unsigned_8;

    FilePath: String_Access;
    Encrypt : Boolean     := True;
    Key     : Unsigned_64 :=    0;
    
    F       : Byte_IO.File_Type;
    OutFile : IO_64.File_Type;
    OutFileName: String_Access;

    BlockArr: Byte_Array;
    Byte    : Unsigned_8;
    Block64 : Unsigned_64;
    BytePos : Integer range 1 .. 8;
                
    Keys    : Keys_Array;

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
    L0:     Unsigned_32 := 0; 
    R0:     Unsigned_32 := 0; 
    Temp:   Unsigned_32 := 0;
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
    FinalBlock: Unsigned_64 := 0;  
    begin
        for I in 1 .. 8 loop
            Block := Block or shift_left(Unsigned_64(BlockArr(I)), (8-I)*8); 
        end loop;
        -- initial perm
        Block := Permutation.IP(Block);
        -- rounds
        Block := Rounds(Block);
        -- Final Permutation
        Block := Permutation.LP (Block);
        for I in 1 .. 8 loop
            FinalBlock := FinalBlock or Shift_Left(Shift_Right(Block, (I-1)*8) and 16#FF#, (8-I)*8); 
        end loop;
        return FinalBlock;
    end AssembleBlock;  

    procedure Usage is
    begin
        Put_Line ("Usage: ades -e -f <file> --key=<key>");
        Put_Line (" -e          Encrypt mode");
        Put_Line (" -d          Decrypt mode");
        Put_Line (" -f <file>   Filepath");
        Put_Line (" --Key       64bit Hexadecimal key");
        Put_Line (" -h --help");
    end Usage;

begin
    begin
    loop
        case Getopt("e d f: h -key= -help") is
            when 'e'=>
                Encrypt := True;
            when 'd'=>
                Encrypt := False;
            when 'f'=>
                FilePath := new String'(Parameter);
            when 'h'=>
                Usage; return;
            when '-' =>
                if Full_Switch = "-key" then
                    Put_Line ("Key=" & Parameter);
                    begin
                        Key := Unsigned_64'Value ("16#" & Parameter & "#");
                    exception
                        when Constraint_Error => 
                        Put_Line ("Keys must be 64 bits long, inserted in hexadecimal");
                        return;
                    end;
                elsif Full_Switch = "-help" then
                    Usage; return;
                end if;
            when others =>
                exit;
        end case;
    end loop;
    exception
        when GNAT.COMMAND_LINE.INVALID_SWITCH => 
        Put_Line ("Invalid argument: " & Full_Switch); return;
    end;

    if FilePath = null then
        put_line ("File path not defined");
        return;
    end if;

    Put_Line ("Plaintext filename:");
    Put_Line (FilePath.all);

    Put ("Key (In Hex): ");
    U64_IO.Put( Key, Base=>16);
    New_Line;

    Byte_IO.open (F, Byte_IO.In_File, FilePath.all);

    for I in 0..9999 loop
        if not (Exists ("e_" & Ada.Strings.Fixed.Trim(Integer'Image (I), Ada.Strings.Left))) then
            if Encrypt then
                OutFileName := new String'("e_" & Ada.Strings.Fixed.Trim(Integer'Image (I), Ada.Strings.Left));
            else
                OutFileName := new String'("d_" & Ada.Strings.Fixed.Trim(Integer'Image (I), Ada.Strings.Left));
            end if;
            exit;
        end if;
    end loop;

    IO_64.Create(OutFile, IO_64.Out_File, OutFileName.all);
    
    Keys := KeysGen(Key, Encrypt);

    BytePos := 1;
    while not Byte_IO.End_Of_File (F) loop
        Byte_IO.read(F, Byte);
        BlockArr(BytePos) := Byte;
        if BytePos = 8 then
            Block64 := AssembleBlock(BlockArr);
            --U64_IO.Put(Block64, Base=>16);New_Line;
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
    Put_Line ("Encryption complete");
   
end Main;
