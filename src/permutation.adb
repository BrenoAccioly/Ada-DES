package body Permutation is

    -- Initial Permutation
    function IP (Block: Unsigned_64) return Unsigned_64 is
        NewBlock: Unsigned_64;
    begin
        NewBlock := 0;
        for I in 1 .. 64 loop
            NewBlock := NewBlock or (shift_left(1 and shift_right(Block, IP_Arr(I)-1), I-1));
        end loop;
        return NewBlock;
    end IP;

    -- Initial Permutation
    function LP (Block: Unsigned_64) return Unsigned_64 is
        NewBlock: Unsigned_64;
    begin
        NewBlock := 0;
        for I in 1 .. 64 loop
            NewBlock := NewBlock or (shift_left(1 and shift_right(Block, LP_Arr(I)-1), I-1));
        end loop;
        return NewBlock;
    end LP;

end Permutation;