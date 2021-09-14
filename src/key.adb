with Ada.Text_IO;
package body Key is
    package IO is new Ada.Text_IO.Integer_IO (Integer_64);
    package U64_IO is new Ada.Text_IO.Modular_IO (Interfaces.unsigned_64);

    function KeyPC1(Key: Unsigned_64) return Unsigned_64 is
    permutedKey: Unsigned_64 := 0;  
    begin
        for I in 1 .. 56 loop
            permutedKey := permutedKey or (shift_left(1 and shift_right(Key, 64-PC1(I)), 56-I));
        end loop;
        return permutedKey;
    end KeyPC1;

    function KeyPC2(Key: Unsigned_64) return Unsigned_64 is
    permutedKey: Unsigned_64 := 0;  
    begin
        for I in 1 .. 48 loop
            permutedKey := permutedKey or (shift_left(1 and shift_right(Key, 56-PC2(I)), 48-I));
        end loop;
        return permutedKey;
    end KeyPC2;

    -- Rotates a 28 bit number
    function rotate_left_28 (Value: Unsigned_32; Amount: Natural) return Unsigned_32 is
    begin 
        return (Shift_Left(Value, Amount) or Shift_Right (Value, 28 - Amount)) and 16#FFFFFFF#;
    end rotate_left_28;

    function KeysGen(Key: Unsigned_64) return Keys_Array is
    Keys: Keys_Array;
    K: Unsigned_64 := 0;  
    C: Unsigned_32 := 0;
    D: Unsigned_32 := 0;
    begin

        --Put_Line ("Key:");
        --U64_IO.Put(
        --    Unsigned_64 (Key),
        --    Base=>16
        --);
        --New_Line;

        K := KeyPC1(Key);
        --IO.put(Integer_64 (K), Base => 16);
        C := Unsigned_32 (shift_right(K, 28));
        D := Unsigned_32 (K and 16#FFFFFFF#);


        for I in 1 .. 16 loop
            C := rotate_left_28(C, Shifts(I));
            D := rotate_left_28(D, Shifts(I));
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
