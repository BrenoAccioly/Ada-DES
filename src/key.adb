with Ada.Text_IO;
package body Key is
    package IO is new Ada.Text_IO.Integer_IO (Integer_64);
    function KeyPC1(Key: Unsigned_64) return Unsigned_64 is
    permutedKey: Unsigned_64 := 0;  
    begin
        for I in 1 .. 56 loop
            permutedKey := permutedKey or (shift_left(1 and shift_right(Key, PC1(I)-1), I-1));
        --put(integer'image(PC1(I)));IO.put(Integer_64 (shift_right(Key, PC1(I)-1)), Base => 2);
        --New_Line;
        end loop;
        return permutedKey;
    end KeyPC1;

    function KeyPC2(Key: Unsigned_64) return Unsigned_64 is
    permutedKey: Unsigned_64 := 0;  
    begin
        for I in 1 .. 48 loop
            permutedKey := permutedKey or (shift_left(1 and shift_right(Key, PC2(I)-1), I-1));
        end loop;
        return permutedKey;
    end KeyPC2;

    function KeysGen(Key: Unsigned_64) return Keys_Array is
    Keys: Keys_Array;
    K: Unsigned_64 := 0;  
    C: Unsigned_32 := 0;
    D: Unsigned_32 := 0;
    begin

        --IO.put(Integer_64 (Key), Base => 2);
        --New_Line;
        K := KeyPC1(Key);
        C := Unsigned_32 (shift_right(K, 28));
        D := Unsigned_32 (K and 16#FFFFFFF#);

        for I in 1 .. 16 loop
            C := rotate_left(C, Shifts(I));
            D := rotate_left(D, Shifts(I));
            K := shift_left(Unsigned_64 (C), 28) or Unsigned_64 (D);
            K := KeyPC2(K);
            Keys(I) := K;
            --New_Line;
            --IO.put(Integer_64 (K), Base => 2);
            --Put_line(Unsigned_64'image (k));
        end loop;

        return Keys;
    end KeysGen;      

end Key;
