||| Packrat monadic parser combinators library
||| Directly translated from http://bford.info/pub/lang/packrat-icfp02/
module Text.Parse

import Text.Pos
import Text.ParseError
import Data.List

%access public export

-- Data types

data Result : (v : Type) -> (d : Type) -> Type where
     Parsed : v -> d -> ParseError -> Result v d
     NoParse : ParseError -> Result v d

data Parser : (d : Type) -> (v : Type) -> Type where
  MkParser : (d -> Result v d) -> Parser d v


interface Derivs d where
  dvPos  : d -> Pos String
  dvChar : d -> Result Char d


nullError : Derivs d => d -> ParseError
nullError dvs = MkParseError (dvPos dvs) []

expError : Derivs d => d  -> String -> ParseError
expError dvs desc = MkParseError (dvPos dvs) [Expected desc]

eofError : (Derivs d) => d -> ParseError
eofError dvs = MkParseError (dvPos dvs) [ Msg "end of input" ]

expected : Derivs d => String -> Parser d v
expected desc = MkParser (\dvs => NoParse (expError dvs desc))

failAt : Derivs d => Pos String -> String -> Parser d v
failAt pos msg = MkParser (\dvs => NoParse (msgError pos msg))

-- -- Basic Combinators

infixl 1 <?>
infixl 1 <?!>

implementation (Derivs d) => Functor (Parser d) where
  map f (MkParser p) = MkParser $ \ v => case p v of
                                           Parsed v d err => Parsed (f v) d err
                                           NoParse err => NoParse err

implementation Derivs d => Applicative (Parser d) where
  pure x = MkParser (\dvs => Parsed x dvs (nullError dvs))

  (MkParser pf) <*> (MkParser pa) =
    MkParser (\ dvs =>
               case pf dvs of
                 Parsed f rem err  =>
                   case pa rem of
                     Parsed x rem' err' => Parsed (f x) rem' (joinErrors err err')
                     NoParse errs => NoParse errs
                 NoParse errs => NoParse errs)

implementation Derivs d => Monad (Parser d) where
  (MkParser p1) >>= f = MkParser parse
    where
      second : ParseError -> Result b d -> Result b d
      second err1 (Parsed val rem err) =
             Parsed val rem (joinErrors err1 err)
      second err1 (NoParse err) =
             NoParse (joinErrors err1 err)

      first : Result a d -> Result b d
      first (Parsed val rem err) =
        let MkParser p2 = f val
        in second err (p2 rem)
      first (NoParse err) = NoParse err

      parse : d -> Result b d
      parse dvs = first (p1 dvs)


fail : Derivs d => String -> Parser d v
fail msg = MkParser (\dvs => NoParse (msgError (dvPos dvs) msg))

unexpected : Derivs d => String -> Parser d v
unexpected str = fail ("unexpected " ++ str)

implementation Derivs d => Alternative (Parser d) where
  empty = MkParser $ \ dv => NoParse (MkParseError (dvPos dv) [ Msg "Nothing to parse" ])

  (MkParser p1) <|> (MkParser p2) = MkParser parse
      where second : ParseError -> Result a d -> Result a d
            second err1 (Parsed val rem err) =
                   Parsed val rem (joinErrors err1 err)
            second err1 (NoParse err) =
                   NoParse (joinErrors err1 err)

            first : d -> Result a d -> Result a d
            first dvs result@(Parsed x y z) = result
            first dvs (NoParse err) = second err (p2 dvs)

            parse : d -> Result a d
            parse dvs = first dvs (p1 dvs)


satisfy : Derivs d => Parser d v -> (v -> Bool) -> Parser d v
satisfy (MkParser p) test = MkParser parse
    where
      check : d -> Result v d -> Result v d
      check dvs (result @ (Parsed val rem err)) =
        if test val
         then result
        else NoParse (expError dvs "a character satisfying predicate")
      check dvs none = none

      parse : d -> Result v d
      parse dvs = check dvs (p dvs)

notFollowedBy : (Derivs d, Show v) => Parser d v -> Parser d ()
notFollowedBy (MkParser p) = MkParser parse
  where
    parse dvs = case (p dvs) of
      Parsed val rem err => NoParse (expError dvs $ "unexpected " ++ show val)
      NoParse err => Parsed () dvs (nullError dvs)

between : Derivs d => Parser d a -> Parser d b -> Parser d v -> Parser d v
between start end content = start *> content <* end

optional : Derivs d => Parser d v -> Parser d (Maybe v)
optional p =
  (do v <- p; pure (Just v)) <|> pure Nothing

many : Derivs d => Parser d v -> Parser d (List v)
many p =
  (do { v <- p; vs <- many p; pure (v :: vs) } ) <|> pure []

many1 : Derivs d => Parser d v -> Parser d (List v)
many1 p = do { v <- p; vs <- many p; pure (v :: vs) }

sepBy1 : Derivs d => Parser d v -> Parser d vsep -> Parser d (List v)
sepBy1 p psep = do v <- p
                   vs <- many (do { psep; p })
                   pure (v :: vs)

sepBy : Derivs d => Parser d v -> Parser d vsep -> Parser d (List v)
sepBy p psep = sepBy1 p psep <|> pure []

endBy : Derivs d => Parser d v -> Parser d vend -> Parser d (List v)
endBy p pend = many (do { v <- p; pend; pure v })

endBy1 : Derivs d => Parser d v -> Parser d vend -> Parser d (List v)
endBy1 p pend = many1 (do { v <- p; pend; pure v })

sepEndBy1 : Derivs d => Parser d v -> Parser d vsep -> Parser d (List v)
sepEndBy1 p psep = do v <- sepBy1 p psep; optional psep; pure v

sepEndBy : Derivs d => Parser d v -> Parser d vsep -> Parser d (List v)
sepEndBy p psep = do v <- sepBy p psep; optional psep; pure v

-- -- chainl1 :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> Parser d v
-- -- chainl1 p psep =
-- --   let psuffix z = (do f <- psep
-- --           v <- p
-- --           psuffix (f z v))
-- --       <|> return z
-- --   in do v <- p
-- --         psuffix v

-- -- chainl :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> v -> Parser d v
-- -- chainl p psep z = chainl1 p psep <|> return z

-- -- chainr1 :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> Parser d v
-- -- chainr1 p psep = (do v <- p
-- --          f <- psep
-- --          w <- chainr1 p psep
-- --          return (f v w))
-- --      <|> p

-- -- chainr :: Derivs d => Parser d v -> Parser d (v -> v -> v) -> v -> Parser d v
-- -- chainr p psep z = chainr1 p psep <|> return z

-- -- choice :: Derivs d => [Parser d v] -> Parser d v
-- -- choice [p] = p
-- -- choice (p:ps) = p <|> choice ps


-- -- -- Error handling

||| Annotate a parser with a description of the construct to be parsed.
||| The resulting parser yields an "expected" error message
||| if the construct cannot be parsed
||| and if no error information is already available
||| indicating a position farther right in the source code
||| (which would normally be more localized/detailed information).
(<?>) : Derivs d => Parser d v -> String -> Parser d v
(MkParser p) <?> desc = MkParser (\ dvs => munge dvs (p dvs))
  where
    fix : d -> ParseError -> ParseError
    fix dvs err@(MkParseError pos ms) =
      if pos > dvPos dvs
      then err
      else expError dvs desc

    munge dvs (Parsed v rem err) = Parsed v rem (fix dvs err)
    munge dvs (NoParse err) =  NoParse (fix dvs err)

||| Stronger version of the <?> error annotation operator.
||| which unconditionally overrides any existing error information.
(<?!>) : Derivs d => Parser d v -> String -> Parser d v
(MkParser p) <?!> desc = MkParser (\dvs => munge dvs (p dvs))
  where
    fix : d -> ParseError -> ParseError
    fix dvs (err @ (MkParseError p ms)) = expError dvs desc

    munge dvs (Parsed v rem err) = Parsed v rem (fix dvs err)
    munge dvs (NoParse err) = NoParse (fix dvs err)


-- Character-oriented parsers

anyChar : Derivs d => Parser d Char
anyChar = MkParser dvChar

char : Derivs d => Char -> Parser d Char
char ch = satisfy anyChar (\c => c == ch) <?> show ch

oneOf : Derivs d => List Char -> Parser d Char
oneOf chs = satisfy anyChar (\c => c `elem` chs)
      <?> ("one of the characters " ++ pack chs)

noneOf : Derivs d => List Char -> Parser d Char
noneOf chs = satisfy anyChar (\c => not (c `elem` chs))
       <?> ("any character not in " ++ pack chs)

string : Derivs d => String -> Parser d String
string str = p (unpack str) <?> show str
  where p : (Derivs d) => List Char -> Parser d String
        p [] = pure str
        p (ch :: chs) = do
          char ch
          p chs

stringFrom : Derivs d => List String -> Parser d String
stringFrom [str] = string str
stringFrom (str :: strs) = string str <|> stringFrom strs

upper : Derivs d => Parser d Char
upper = satisfy anyChar isUpper <?> "uppercase letter"

lower : Derivs d => Parser d Char
lower = satisfy anyChar isLower <?> "lowercase letter"

letter : Derivs d => Parser d Char
letter = satisfy anyChar isAlpha <?> "letter"

alphaNum : Derivs d => Parser d Char
alphaNum = satisfy anyChar isAlphaNum <?> "letter or digit"

digit : Derivs d => Parser d Char
digit = satisfy anyChar isDigit -- <?> "digit"

hexDigit : Derivs d => Parser d Char
hexDigit = satisfy anyChar isHexDigit <?> "hexadecimal digit (0-9, a-f)"

octDigit :  Derivs d => Parser d Char
octDigit = satisfy anyChar isOctDigit <?> "octal digit (0-7)"

newline :  Derivs d => Parser d Char
newline = char '\n'

tab : Derivs d => Parser d Char
tab = char '\t'

space : Derivs d => Parser d Char
space = satisfy anyChar isSpace <?> "whitespace character"


spaces : Derivs d => Parser d (List Char)
spaces = many space

eof : Derivs d => Parser d ()
eof = notFollowedBy anyChar <?> "end of input"


-- -- State manipulation

-- getDerivs :: Derivs d => Parser d d
-- getDerivs = Parser (\dvs -> Parsed dvs dvs (nullError dvs))

-- setDerivs :: Derivs d => d -> Parser d ()
-- setDerivs newdvs = Parser (\dvs -> Parsed () newdvs (nullError dvs))

-- getPos :: Derivs d => Parser d Pos
-- getPos = Parser (\dvs -> Parsed (dvPos dvs) dvs (nullError dvs))


-- -- Special function that converts a Derivs "back" into an ordinary String
-- -- by extracting the successive dvChar elements.
-- dvString :: Derivs d => d -> String
-- dvString d =
--   case dvChar d of
--     NoParse err -> []
--     Parsed c rem err -> (c : dvString rem)
