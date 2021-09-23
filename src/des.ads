with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Sequential_IO;
with Key; use Key;
with Permutation;
with Feistel;
package DES is
    Max_Length: constant Integer := 8;
    ECB_MODE  : constant Natural := 2#0#;
    CBC_MODE  : constant Natural := 2#1#;
    type Byte_Array is array(1 .. Max_Length) of Unsigned_8;
    package Byte_IO is new Ada.Sequential_IO (Unsigned_8);
    package IO_64   is new Ada.Sequential_IO (Unsigned_64);
    package U64_IO  is new Ada.Text_IO.Modular_IO (Interfaces.unsigned_64);

    procedure EncryptFile(Encrypt: Boolean; Key: Unsigned_64; InFileName: String; OutFileName: String; ModeOfOperation: Natural; IV: Unsigned_64);
end DES;