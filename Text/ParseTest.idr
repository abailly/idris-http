module Text.ParseTest

import Text.Parse
import Text.Pos
import System


-- * Example

record Arithmetics where
  constructor MkArith
  dvDecimal : Lazy (Result Int Arithmetics)
  dvChar		: Lazy (Result Char Arithmetics)
  dvPos		: Lazy (Pos String)

Derivs Arithmetics where
  dvChar d = dvChar d
  dvPos d = dvPos d

pDecimal : Parser Arithmetics Int
pDecimal = do
  c <- digit
  pure (cast c - 32)

parseAr : (Pos String) -> String -> Arithmetics
parseAr pos s = d
  where
    mutual
      d : Arithmetics
      d    = MkArith dec chr pos

      dec : (Result Int Arithmetics)
      dec  = let MkParser p = pDecimal
             in p d

      chr : (Result Char Arithmetics)
      chr  = case unpack s of
               (c :: s') => Parsed c (parseAr (nextPos pos c) $ pack s') (nullError d)
               [] => NoParse (eofError d)

eval : String -> Either String Int
eval s = case Force (dvDecimal (parseAr (MkPos "<input>" 1 1) s)) of
              Parsed v d' e' => Right v
              NoParse err => Left $ show err

-- * Tests

shouldBe : (Eq a, Show a) => a -> a -> IO (Either String ())
shouldBe a a' =
  if a /= a'
  then pure $ Left ("got " ++ show a ++ ", expected " ++ show a')
  else pure $ Right ()

test_canParseASingleDigitString : IO (Either String ())
test_canParseASingleDigitString = do
  let res = eval "1"
  res `shouldBe` Right 1

export
test : IO ()
test = do
  Right () <- test_canParseASingleDigitString
    | Left err => putStrLn err ; exit 1
