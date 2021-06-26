with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;

package Key is
    
    type Keys_Array is array(1 .. 16) of Unsigned_64;
    type Shifts_Array is array(1 .. 16) of Integer;
    type PC1_Array is array(1 .. 56) of Integer;
    type PC2_Array is array(1 .. 48) of Integer;

    PC1: constant PC1_Array := 
    (
        57, 49, 41, 33, 25, 17,  9,
         1, 58, 50, 42, 34, 26, 18,
        10,  2, 59, 51, 43, 35, 27,
        19, 11,  3, 60, 52, 44, 36,

        63, 55, 47, 39, 31, 23, 15,
         7, 62, 54, 46, 38, 30, 22,
        14,  6, 61, 53, 45, 37, 29,
        21, 13,  5, 28, 20, 12,  4 
    );

    PC2: constant PC2_Array := 
    (
        14, 17, 11, 24,  1,  5,
         3, 28, 15,  6, 21, 10,
        23, 19, 12,  4, 26,  8,
        16,  7, 27, 20, 13,  2,
        41, 52, 31, 37, 47, 55,
        30, 40, 51, 45, 33, 48,
        44, 49, 39, 56, 34, 53,
        46, 42, 50, 36, 29, 32
    );

    Shifts: constant Shifts_Array :=
    (
        1, 1, 2, 2, 2, 2, 2, 2,
        1, 2, 2, 2, 2, 2, 2, 1
    );

    function KeysGen(Key: Unsigned_64) return Keys_Array;

end Key;