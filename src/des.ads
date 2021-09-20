with Interfaces; use Interfaces;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Sequential_IO;
with Key; use Key;
with Permutation;
with Feistel;
package DES is
    Max_Length: constant Integer := 8;
    type Byte_Array is array(1 .. Max_Length) of Unsigned_8;
    package Byte_IO is new Ada.Sequential_IO (Unsigned_8);
    package IO_64   is new Ada.Sequential_IO (Unsigned_64);

    function InsertPaddingCMS(BlockArr: Byte_Array; Bytes: Natural) return Byte_Array;
    function DES_Function(R: Unsigned_32; K: Unsigned_64)           return Unsigned_32;
    function Rounds(Block: Unsigned_64; Keys: Keys_Array)           return Unsigned_64;
    function AssembleBlock(BlockArr: Byte_Array; Keys: Keys_Array)  return Unsigned_64;
    procedure EncryptFile(Encrypt: Boolean; Key: Unsigned_64; InFileName: String; OutFileName: String);
end DES;