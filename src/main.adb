with GNAT.Command_Line;       use GNAT.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;
with Interfaces;              use Interfaces;
with GNAT.Strings;            use GNAT.Strings;
with Ada.Directories;         use Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Sequential_IO;

with DES;                     use DES;

procedure Main is 
    package U64_IO is new Ada.Text_IO.Modular_IO (Interfaces.unsigned_64);

    InPath         : String_Access;
    OutPath        : String_Access;
    Encrypt        : Boolean     := True;
    Key            : Unsigned_64 :=    0;
    IV             : Unsigned_64 :=    0;
    ModeOfOperation: Natural     := 2#0#;

    procedure Usage is
    begin
        Put_Line ("Usage: ades -e -f <file> --key=<key>");
        Put_Line (" -e          Encrypt mode");
        Put_Line (" -d          Decrypt mode");
        Put_Line (" -f <file>   Input Filepath");
        Put_Line (" -o <file>   Output Filepath");
        Put_Line (" --key=<key> 64 bit Hexadecimal key");
        Put_Line (" --ecb       Uses ECB mode of operation");
        Put_Line (" --cbc=<iv>  Uses CBC mode, argument is a 64 bit initialization vector");
        Put_Line (" -h --help");
    end Usage;

begin
    begin
    loop
        case Getopt("e d f: o: h -key= -ecb -cbc= -help") is
            when 'e'=> Encrypt := True;
            when 'd'=> Encrypt := False;
            when 'f'=> InPath  := new String'(Parameter);
            when 'o'=> OutPath := new String'(Parameter);
            when 'h'=> Usage; return;
            when '-' =>
                if Full_Switch = "-key" then
                    begin
                        Key := Unsigned_64'Value ("16#" & Parameter & "#");
                    exception
                        when Constraint_Error => 
                        Put_Line ("Keys must be 64 bits long, inserted in hexadecimal");
                        return;
                    end;
                elsif Full_Switch = "-ecb" then
                    ModeOfOperation := DES.ECB_MODE;
                elsif Full_Switch = "-cbc" then
                    ModeOfOperation := DES.CBC_MODE;
                    begin
                        IV := Unsigned_64'Value ("16#" & Parameter & "#");
                    exception
                        when Constraint_Error => 
                        Put_Line ("Initialization vector must be 64 bits long, inserted in hexadecimal");
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

    if InPath = null then
        put_line ("File path not defined"); return;
    end if;
    if OutPath = null then
        if Encrypt then 
            OutPath := new String'("e_");
        else 
            OutPath := new String'("d_");
        end if;
        for I in 0..9999 loop
            if not (Exists (OutPath.all & Ada.Strings.Fixed.Trim(Integer'Image (I), Ada.Strings.Left))) then
                OutPath := new String'(OutPath.all & Ada.Strings.Fixed.Trim(Integer'Image (I), Ada.Strings.Left));
                exit;
            end if;
        end loop;
        if(OutPath = Null) then return; end if;
    end if;

    Put ("Input  filename: "); Put (InPath.all); New_Line;
    Put ("Output filename: "); Put (OutPath.all); New_Line;
    Put ("Key (In Hex)   : "); U64_IO.Put( Key, Base=>16); New_Line;
    
    DES.EncryptFile (Encrypt, Key, InPath.all, OutPath.all, ModeOfOperation, IV);

    if Encrypt then
        Put_Line ("Encryption complete");
    else
        Put_Line ("Decryption complete");
    end if;
   
end Main;
