||| Packrat monadic parser combinators library
||| Directly translated from http://bford.info/pub/lang/packrat-icfp02/
module Text.Parse

import Text.Pos
import Data.List

%access public export

-- Data types

data Message : Type where
  Expected : String -> Message
  Msg : String -> Message

record ParseError  where
  constructor MkParseError
  errorPos : Pos String -- TODO generalise source
  errorMessages  : List Message

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

Show ParseError where
  show (MkParseError pos []) =
    show pos ++ ": unknown error"
  show (MkParseError pos msgs) = expectmsg expects ++ messages msgs
     where
       mutual
         expects : List String
         expects = getExpects msgs

         getExpects : List Message -> List String
         getExpects [] = []
         getExpects (Expected exp :: rest) = exp :: getExpects rest
         getExpects (Msg msg :: rest) = getExpects rest

         expectmsg : List String -> String
         expectmsg [] = ""
         expectmsg [exp] = show pos ++ ": expecting " ++ exp ++ "\n"
         expectmsg [e1, e2] = show pos ++ ": expecting either "
               ++ e1 ++ " or " ++ e2 ++ "\n"
         expectmsg (first :: rest) = show pos ++ ": expecting one of: "
                 ++ first ++ expectlist rest
                 ++ "\n"

         expectlist : List String -> String
         expectlist [last] = ", or " ++ last
         expectlist (mid :: rest) = ", " ++ mid ++ expectlist rest

         messages : List Message -> String
         messages [] = ""
         messages (Expected exp :: rest) = messages rest
         messages (Msg msg :: rest) =
           show pos ++ ": " ++ msg ++ "\n" ++ messages rest

||| Potentially join two sets of ParseErrors,
||| but only if the position didn't change from the first to the second.
||| If it did, just return the "new" (second) set of errors.
joinErrors : ParseError -> ParseError -> ParseError
joinErrors (MkParseError p m) (MkParseError p' m') =
  if p' > p || isNil  m
  then MkParseError p' m'
    else if p > p' || isNil  m'
         then MkParseError p m
         else MkParseError p (m ++ m')

msgError : Pos String -> String -> ParseError
msgError pos msg = MkParseError pos [Msg msg]

-- -- Basic Combinators

--infixl 2 <|>
--infixl 1 <?>
--infixl 1 <?!>

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

implementation Derivs d => Alternative (Parser d) where
  empty = MkParser $ \ dv => NoParse (MkParseError (dvPos dv) [ Msg "Nothing to parse" ])

  (MkParser p1) <|> (MkParser p2) = MkParser parse
      where second : ParseError -> Result a d -> Result a d
            second err1 (Parsed val rem err) =
                   Parsed val rem (joinErrors err1 err)
            second err1 (NoParse err) =
                   NoParse (joinErrors err1 err)

            -- first : Result a d -> Result b d
            -- first dvs (result @ (Parsed val rem err)) = result
            -- first dvs (NoParse err) = second err (p2 dvs)

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
        else NoParse (nullError dvs)
      check dvs none = none

      parse : d -> Result v d
      parse dvs = check dvs (p dvs)

-- -- notFollowedBy :: (Derivs d, Show v) => Parser d v -> Parser d ()
-- -- notFollowedBy (Parser p) = Parser parse
-- --   where parse dvs = case (p dvs) of
-- --     Parsed val rem err ->
-- --       NoParse (msgError (dvPos dvs)
-- --             ("unexpected " ++ show val))
-- --     NoParse err -> Parsed () dvs (nullError dvs)

-- -- optional :: Derivs d => Parser d v -> Parser d (Maybe v)
-- -- optional p = (do v <- p; return (Just v)) <|> return Nothing

-- -- many :: Derivs d => Parser d v -> Parser d [v]
-- -- many p = (do { v <- p; vs <- many p; return (v : vs) } )
-- --    <|> return []

-- -- many1 :: Derivs d => Parser d v -> Parser d [v]
-- -- many1 p = do { v <- p; vs <- many p; return (v : vs) }

-- -- sepBy1 :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]
-- -- sepBy1 p psep = do v <- p
-- --        vs <- many (do { psep; p })
-- --        return (v : vs)

-- -- sepBy :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]
-- -- sepBy p psep = sepBy1 p psep <|> return []

-- -- endBy :: Derivs d => Parser d v -> Parser d vend -> Parser d [v]
-- -- endBy p pend = many (do { v <- p; pend; return v })

-- -- endBy1 :: Derivs d => Parser d v -> Parser d vend -> Parser d [v]
-- -- endBy1 p pend = many1 (do { v <- p; pend; return v })

-- -- sepEndBy1 :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]
-- -- sepEndBy1 p psep = do v <- sepBy1 p psep; optional psep; return v

-- -- sepEndBy :: Derivs d => Parser d v -> Parser d vsep -> Parser d [v]
-- -- sepEndBy p psep = do v <- sepBy p psep; optional psep; return v

-- -- chainl1 :: Derivs d => Parser d v -> Parser d (v->v->v) -> Parser d v
-- -- chainl1 p psep =
-- --   let psuffix z = (do f <- psep
-- --           v <- p
-- --           psuffix (f z v))
-- --       <|> return z
-- --   in do v <- p
-- --         psuffix v

-- -- chainl :: Derivs d => Parser d v -> Parser d (v->v->v) -> v -> Parser d v
-- -- chainl p psep z = chainl1 p psep <|> return z

-- -- chainr1 :: Derivs d => Parser d v -> Parser d (v->v->v) -> Parser d v
-- -- chainr1 p psep = (do v <- p
-- --          f <- psep
-- --          w <- chainr1 p psep
-- --          return (f v w))
-- --      <|> p

-- -- chainr :: Derivs d => Parser d v -> Parser d (v->v->v) -> v -> Parser d v
-- -- chainr p psep z = chainr1 p psep <|> return z

-- -- choice :: Derivs d => [Parser d v] -> Parser d v
-- -- choice [p] = p
-- -- choice (p:ps) = p <|> choice ps


-- -- -- Error handling
-- -- instance Eq Message where
-- --   Expected e1 == Expected e2  = e1 == e2
-- --   Message m1 == Message m2  = m1 == m2
-- --   _ == _        = False

-- -- failAt :: Derivs d => Pos -> String -> Parser d v
-- -- failAt pos msg = Parser (\dvs -> NoParse (msgError pos msg))

-- -- -- Annotate a parser with a description of the construct to be parsed.
-- -- -- The resulting parser yields an "expected" error message
-- -- -- if the construct cannot be parsed
-- -- -- and if no error information is already available
-- -- -- indicating a position farther right in the source code
-- -- -- (which would normally be more localized/detailed information).
-- -- (<?>) :: Derivs d => Parser d v -> String -> Parser d v
-- -- (Parser p) <?> desc = Parser (\dvs -> munge dvs (p dvs))
-- --       where munge dvs (Parsed v rem err) =
-- --         Parsed v rem (fix dvs err)
-- --             munge dvs (NoParse err) =
-- --         NoParse (fix dvs err)
-- --             fix dvs (err @ (ParseError p ms)) =
-- --         if p > dvPos dvs then err
-- --         else expError (dvPos dvs) desc

-- -- -- Stronger version of the <?> error annotation operator above,
-- -- -- which unconditionally overrides any existing error information.
-- -- (<?!>) :: Derivs d => Parser d v -> String -> Parser d v
-- -- (Parser p) <?!> desc = Parser (\dvs -> munge dvs (p dvs))
-- --       where munge dvs (Parsed v rem err) =
-- --         Parsed v rem (fix dvs err)
-- --             munge dvs (NoParse err) =
-- --         NoParse (fix dvs err)
-- --             fix dvs (err @ (ParseError p ms)) =
-- --         expError (dvPos dvs) desc

-- -- -- Potentially join two sets of ParseErrors,
-- -- -- but only if the position didn't change from the first to the second.
-- -- -- If it did, just return the "new" (second) set of errors.
-- -- joinErrors (e @ (ParseError p m)) (e' @ (ParseError p' m')) =
-- --   if p' > p || null m then e'
-- --   else if p > p' || null m' then e
-- --   else ParseError p (m `union` m')

-- -- nullError dvs = ParseError (dvPos dvs) []

-- -- expError pos desc = ParseError pos [Expected desc]


eofError : (Derivs d) => d -> ParseError
eofError dvs = MkParseError (dvPos dvs) [ Msg "end of input" ]

-- -- expected :: Derivs d => String -> Parser d v
-- -- expected desc = Parser (\dvs -> NoParse (expError (dvPos dvs) desc))

-- -- unexpected :: Derivs d => String -> Parser d v
-- -- unexpected str = fail ("unexpected " ++ str)


-- -- -- Comparison operators for ParseError just compare relative positions.
-- -- instance Eq ParseError where
-- --   ParseError p1 m1 == ParseError p2 m2  = p1 == p2
-- --   ParseError p1 m1 /= ParseError p2 m2  = p1 /= p2

-- -- instance Ord ParseError where
-- --   ParseError p1 m1 < ParseError p2 m2  = p1 < p2
-- --   ParseError p1 m1 > ParseError p2 m2  = p1 > p2
-- --   ParseError p1 m1 <= ParseError p2 m2  = p1 <= p2
-- --   ParseError p1 m1 >= ParseError p2 m2  = p1 >= p2

-- --   -- Special behavior: "max" joins two errors
-- --   max p1 p2 = joinErrors p1 p2
-- --   min p1 p2 = undefined



-- Character-oriented parsers

anyChar : Derivs d => Parser d Char
anyChar = MkParser dvChar

char : Derivs d => Char -> Parser d Char
char ch = satisfy anyChar (\c => c == ch) --  <?> show ch

-- -- oneOf :: Derivs d => [Char] -> Parser d Char
-- -- oneOf chs = satisfy anyChar (\c -> c `elem` chs)
-- --       <?> ("one of the characters " ++ show chs)

-- -- noneOf :: Derivs d => [Char] -> Parser d Char
-- -- noneOf chs = satisfy anyChar (\c -> not (c `elem` chs))
-- --        <?> ("any character not in " ++ show chs)

string : Derivs d => String -> Parser d String
string str = p (unpack str) -- <?> show str
  where p : (Derivs d) => List Char -> Parser d String
        p [] = pure str
        p (ch :: chs) = do
          char ch
          p chs

-- -- stringFrom :: Derivs d => [String] -> Parser d String
-- -- stringFrom [str] = string str
-- -- stringFrom (str : strs) = string str <|> stringFrom strs

-- -- upper :: Derivs d => Parser d Char
-- -- upper = satisfy anyChar isUpper <?> "uppercase letter"

-- -- lower :: Derivs d => Parser d Char
-- -- lower = satisfy anyChar isLower <?> "lowercase letter"

-- -- letter :: Derivs d => Parser d Char
-- -- letter = satisfy anyChar isAlpha <?> "letter"

-- -- alphaNum :: Derivs d => Parser d Char
-- -- alphaNum = satisfy anyChar isAlphaNum <?> "letter or digit"

digit : Derivs d => Parser d Char
digit = satisfy anyChar isDigit -- <?> "digit"

-- hexDigit :: Derivs d => Parser d Char
-- hexDigit = satisfy anyChar isHexDigit <?> "hexadecimal digit (0-9, a-f)"

-- octDigit :: Derivs d => Parser d Char
-- octDigit = satisfy anyChar isOctDigit <?> "octal digit (0-7)"

-- newline :: Derivs d => Parser d Char
-- newline = char '\n'

-- tab :: Derivs d => Parser d Char
-- tab = char '\t'

-- space :: Derivs d => Parser d Char
-- space = satisfy anyChar isSpace <?> "whitespace character"

-- spaces :: Derivs d => Parser d [Char]
-- spaces = many space

-- eof :: Derivs d => Parser d ()
-- eof = notFollowedBy anyChar <?> "end of input"


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
