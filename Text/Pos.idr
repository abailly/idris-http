-- Simple data type to keep track of character positions
-- within a text file or other text stream.
module Text.Pos

||| A position in a specific character source
||| Packrat parsers work by memoizing results of parsing rules, pointing
||| at some position in the input stream.
public export
record Pos source where
  constructor MkPos
  posFile	: source
  posLine	: Int
  posCol	: Int

-- nextPos (Pos file line col) c =
-- 	if c == '\n' then Pos file (line + 1) 1
-- 	else if c == '\t' then Pos file line ((div (col + 8 - 1) 8) * 8 + 1)
-- 	else Pos file line (col + 1)

public export
Eq (Pos String) where
	MkPos f1 l1 c1 == MkPos f2 l2 c2 =
		f1 == f2 && l1 == l2 && c1 == c2

public export
Ord (Pos String) where
	MkPos f1 l1 c1 <= MkPos f2 l2 c2 =
		(l1 < l2) || (l1 == l2 && c1 <= c2)

public export
Show (Pos String) where
	show (MkPos file line col) = file ++ ":" ++ show line ++ ":" ++ show col


-- showPosRel (Pos file line col) (Pos file' line' col') =
-- 	if (file == file')
-- 	then	if (line == line')
-- 		then "column " ++ show col'
-- 		else "line " ++ show line' ++ ", column " ++ show col'
-- 	else show (Pos file' line' col')
