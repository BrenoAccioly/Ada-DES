package body Permutation is
    function Permute (Block: Unsigned_64; Perm: Perm_Array) return Unsigned_64 is
    NewBlock: Unsigned_64;
    begin
        NewBlock := 0;
        for I in 1 .. 64 loop
            NewBlock := NewBlock or (shift_left(1 and shift_right(Block, Perm(I)-1), I-1));
        end loop;
        return NewBlock;
    end Permute;
end Permutation;