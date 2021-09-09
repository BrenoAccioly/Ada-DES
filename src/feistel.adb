package body Feistel is
    
    -- Expansion
    function Expansion (R: Unsigned_32) return Unsigned_64 is
    Expanded: Unsigned_64;
    begin
        Expanded := 0;
        for I in 1 .. 48 loop
            Expanded := Expanded or (shift_left(1 and shift_right(Unsigned_64 (R), E(I)-1), I-1));
        end loop;
        return Expanded;
    end Expansion;
    
    function GetChunk (E: Unsigned_64; I: Integer) return Unsigned_8 is
    Chunk: Unsigned_8 := 2#00111111#;
    begin
        Chunk := Unsigned_8 (Unsigned_64 (Chunk) and shift_right(E, 6*(I-1)));
        return Chunk;
    end GetChunk;

    function GetRow (Chunk: Unsigned_8) return Unsigned_8 is
    Row: Unsigned_8 := 0;
    begin
        Row := shift_right(Chunk and 2#100000#, 4) or (Chunk and 2#1#);
        return Row; 
    end GetRow;

    function GetCol (Chunk: Unsigned_8) return Unsigned_8 is
    Col: Unsigned_8 := 0;
    begin
        Col := shift_right(Col and 2#011110#, 1);
        return Col;
    end GetCol;

    function S_Boxes (E: Unsigned_64) return Unsigned_32 is
    Result: Unsigned_32 := 0;
    Index: Integer;
    Chunk: Unsigned_8;
    begin
        -- Todo (16x4) (row)x16 + col
        for I in 1 .. 8 loop
            Chunk := GetChunk(E, I);
            --Put_Line ("Chunk: ");
            --Put_Line (Unsigned_8'image (Chunk));
            --Put_Line ("Row: ");
            --Put_Line (Unsigned_8'image (GetRow(Chunk)));
            --Put_Line ("Col: ");
            --Put_Line (Unsigned_8'image (GetCol(Chunk)));
            Index := Integer (GetRow(Chunk)*16 + GetCol(Chunk) + 1);
            --Put_Line (Integer'image (Index));
            Chunk := Unsigned_8 (SBoxes(I)(Index));
            --Put_Line (Unsigned_8'image (Chunk));
            Result := Result or shift_left(Unsigned_32 (Chunk), I * 4);
        end loop;

        return Result; 
    end S_Boxes;


    function Straight (C: Unsigned_32) return Unsigned_32 is
    Output: Unsigned_32 := 0;
    begin
        for I in 1 .. 32 loop
            Output := Output or (shift_left(1 and shift_right(Unsigned_32 (c), P(I)-1), I-1));
        end loop;
        return Output;
    end Straight;

end Feistel;