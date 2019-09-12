module Text.ParseTest

import Text.Parse
import Text.Pos
import System


-- * Example

record Arithmetics where
  constructor MkArith
  arDecimal : Lazy (Result Int Arithmetics)
  arChar : Lazy (Result Char Arithmetics)
  arPos : Pos String

Derivs Arithmetics where
  dvChar d = arChar d
  dvPos d = arPos d

pDecimal : Parser Arithmetics Int
pDecimal = foldl (\ x, c => x * 10 + (cast c - 48)) 0 <$> many1 digit <?> "decimal integer"

parseAr : Pos String -> List Char -> Arithmetics
parseAr pos s = d
  where
    mutual
      d : Arithmetics
      d    = MkArith dec chr pos

      dec : Result Int Arithmetics
      dec  = let MkParser p = pDecimal
             in p d

      chr : Result Char Arithmetics
      chr  = case s of
               (c :: s') => Parsed c (parseAr (nextPos pos c) s') (nullError d)
               [] => NoParse (eofError d)

eval : String -> Either String Int
eval s = case arDecimal (parseAr (MkPos "<input>" 1 1) $ unpack s) of
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

test_canParseAMultipleDigitString : IO (Either String ())
test_canParseAMultipleDigitString = do
  let res = eval "13"
  res `shouldBe` Right 13

test_returnsAnError : IO (Either String ())
test_returnsAnError = do
  eval "z" `shouldBe` Left "<input>:1:1: expecting decimal integer\n"

export
test : IO ()
test = do
  (lefts, _) <- partitionEithers <$> sequence [ test_canParseASingleDigitString
                                              , test_canParseAMultipleDigitString
                                              , test_returnsAnError
                                              ]
  when (lefts /= []) $ do
    traverse putStrLn lefts
    exit 1
