-- to make our Parser type the instance of Monad typeclass
-- and be able to use 'do' costruct in our programs
-- we need import the declaration of this typeclass 
import Control.Monad
import Data.Time


-- ******** THE DEFINITION OF THE BASIC PARSER TYPE **********


-- simple parser type
newtype Parser a = Parser ( String->[(a, String)] )


-- accessor method to the parser function
parse :: Parser a -> (String -> [(a, String)])
parse (Parser f) = f

-- declaring our Parser type as instance of monad typeclass
-- allows us to use beautiful and concise "do-syntax"
-- for combining 2 or more different parsers into one
-- additionally, when our parser becomes monad, we give access
-- to the huge amount of convenient functions from standart library
-- working with monadic types
instance Monad (Parser) where
    return x = Parser $ \str -> [(x, str)]
    p >>= f  = Parser $ \str -> concat [ parse (f a) str' | 
                                       (a, str') <- parse p str]
    -- alternative implementation, using list-monad
    -- actually the implementation is the SAME, only the way
    -- we write it is different. Here i use the list-monad
    -- instead of normal list comprehensions but in the essence
    -- they are the same thing
    --p >>= f  = Parser $ \str -> do 
    --                             (a, str') <- parse p str
    --                             b <- parse (f a) str'
    --                             return b 

-- our parser also act as monoid - we can combine results
-- from 2 or more parser into single one (same as '*' or '+' on natural numbers,
-- for example), and there is also so-called ZERO-element (FAIL-parser, 
-- equal to the 0 im monoid (*,0,N) => 0 * 5 * 2 == 0 , N- is set of natural numbers) 
-- that immediatly stops parsing and returns the empty list (empty list has the meaning of FAIL)

-- it's not necessary to declare our monad to be instance of `MonadPlus` typeclass
-- but there are several point why it's useful:
-- 1) It's a good programming style - ...
-- 2) In the future we might use some functions
-- from the standart library that requires our Parser to
-- be an instance of `MonadPlus`
instance MonadPlus (Parser) where
   mzero = Parser $ \_ -> [] -- `mzero` represents FAIL-parser, that always ends unsuccesfully
   mplus p1 p2 = Parser $ \str -> parse p1 str ++ parse p2 str -- to combine the
   -- results from both p1 and p2 parsers we simply have to merge those lists (with ++) into the single one



-- ******** BASIC PARSERS **********

-- parse any char
-- this parser reads sinlge character from given input string
-- we don't care about the exact value of this char, we just read it
-- and save in result pair
-- then if there's no char left (the parsed string is empty) -> ERROR
item :: Parser Char
item = Parser $ \str -> case str of
                          (x:xs) -> [(x,xs)]
                          [] -> []

-- parse SPECIFIC char
-- if given character is not equal to the 
-- sample 'c', the parser fails
char :: Char -> Parser Char
char c = do
          a <- item
          if a == c
            then
                return a -- OK, successfully parsed
            else
                mzero -- BAD character, stop parsing


-- parse specific char. from range ('c1'-'c2')
rangeChar :: [Char] -> Parser Char
rangeChar range = do
                    c <- item
                    if c `elem` range
                        then return c 
                    else
                        mzero


testSmall = do
              --many1 (char 'z' +++ char 'b')
              xs <- safeMany (char 'b' +++ char 'g')
              x  <- char 'g'
              --empty
              return (xs++[x])

-- a+aa == aa + a
-- (a+b)*b ==> FAILL!!!

-- parse specific string
string :: String -> Parser String
string []     = return []
string (x:xs) = do
                 a <- item -- read next char from string
                 if a == x -- does it correspond to the given string-pattern?
                      then do
                       string xs -- parse other part recursively
                       return (a:xs) -- this char. is OK, save it
                      else
                       mzero -- string-pattern has failed


-- this parser tests whether we are at the end of parsed string or not
empty :: Parser ()
empty = Parser $ \str -> case str of
                          (x:_) -> [] -- more chars to read -> FAIL in parsing
                          [] -> [((),str)] -- return empty result


-- ******** BASIC PARSER COMBINATORS **********


-- deterministic choice combinator
-- if we get more than 1 result, we just take the
-- first one and throw away the rest part of result list
(+++) :: Parser a -> Parser a -> Parser a
(+++) p1 p2 = Parser $ \str -> case parse (p1 `mplus` p2) str of -- first, try to parse both p1 and p2
                                    [] -> [] -- if no results either from p1 or p2 -> PARSER ERROR
                                    (x:_) -> [x] -- choose only FIRST non-empty result


-- parse string 0 or more times with given parser 
-- it's similar to * quantifier used in regular expressions
many :: Parser a -> Parser [a]
many p = do
           m <- many1 p
           return m
         +++
           return [] -- empty result, end of parsing


safeMany :: Parser a -> Parser [a]
safeMany p = do {a  <- p; 
                 xs <- safeMany p; 
                 return (a:xs)} 
             `mplus` 
                 return []

--safeMany1 :: Parser a -> Parser [a]
--safeMany1 p = do
--               {x <- p;
--                xs <- safeMany p;
--                return (x:xs)}

-- almost the same as `many`
-- this is equal to '+' quantifier used in regular expressions
many1 :: Parser a -> Parser [a]
many1 p = do
           x <- p
           xs <- many p
           return (x:xs)
           -- we do not assume the empty result here
           -- as it was in `many` because for `many`
           -- it's an error to have 0 results
           -- it should be at least 1


-- apply given parer 'p' zero or one time
zeroOne :: Parser a -> Parser [a]
zeroOne p = do {res <- p; 
                return [res]}

            +++ (return [])              

-- construct recursive chain of parsers p1 and p2
-- (((p1) p2 p1) p2 p1) ..... (at he beginning, apply p1 parser first ,
-- then p2 and p1, then recursively try (p2 p1) again and again...)
-- There has to be at least p1 in this chain, it could not be empty
-- the correct result could like like this:
-- p1   OR
-- p1 p2 p1    OR
-- p1 p2 p1 p2 p1    OR ..
-- ...... 
-- it's very convenient combinator for parsing
-- the expression looking like this: 2+1+3+6+1+...
--chain1 :: Parser a -> Parser b -> Parser c
--chain1 p1 p2 = p1 +++ {do a <- p2;}



-- ******** GRAMMAR PARSERS (GRAMMAR RULES) **********


-- here we define simple context-free grammar
-- for regular expressions parsing
-- START_REG -> starting rule

-- GRAMMAR
-- {1,0} - means zero or one element
-- +/*   - one or more elements/zero or more elements 

-- START_REG     = BASE_REG 'end_of_string'
-- BASE_REG      = COMPOUND_REG'+'BASE_REG{1,0}
-- COMPOUND_REG  = BRACES_REG COMPOUND_REG{1,0} | CONCAT_REG COMPOUND_REG{1,0}
-- BRACES_REG    = '(' BASE_REG ')' QUANTIFICATOR
-- QUANTIFICATOR = '+' | '*'
-- CONCAT_REG    = SIMPLE_REG+
-- SIMPLE_REG    = 'a' | 'b' | 'c' | ... | 'z'


-- START_REG     = BASE_REG 'end_of_string'
startReg :: Parser String
startReg = do           
             res <- baseReg
             empty -- after the entire parsing process
                   -- input string should be empty
             return res
-- if at the end still have unparsed symbol,
-- then the expression we tried to parse is not
-- compatible with our grammar as we defined it 


-- BASE_REG      = COMPOUND_REG'+'BASE_REG{1,0}
baseReg :: Parser String
baseReg = do 
            cmp <- compoundReg
            plus <- plusReg

            case plus of 

                  [x] -> return (cmp++x++"<!>")
                  []  -> return (cmp)



-- '+'BASE_REG{1,0}
-- returns either single result [String] or empty []
plusReg :: Parser [String]
plusReg = zeroOne (do a <- char '+'
                      b <- baseReg
                      return ([a]++b))


-- COMPOUND_REG = BRACES_REG COMPOUND_REG{1,0} | CONCAT_REG COMPOUND_REG{1,0}
-- the reg. expression can be bounde by braces with qualifier at the end
compoundReg :: Parser String
compoundReg = do 
               {a <- bracesReg baseReg;   -- or it could be compound reg. expression, composed from others reg. expr.
                zo <- zeroOne compoundReg; -- recursivaly parse the compound expression 
                case zo of 
                  [x] -> return (a++x) -- save parsed result string
                  [] -> return (a)}  
              +++
              do 
                {a <- concatReg;   -- or it could be compound reg. expression, composed from others reg. expr.
                 b <- compoundReg; -- recursivaly parse the compound expression 
                 return (a++b)}     -- save parsed result string
              +++ 
              do {concatReg} -- or it could be simple sequence of regular symbols


-- CONCAT_REG    = SIMPLE_REG+
concatReg :: Parser String
concatReg = many1 simpleReg       -- CONCAT_REG = xxxx.... sequence of elementary chars , length >= 1


-- SIMPLE_REG    = 'a' | 'b' | 'c' | ...
simpleReg :: Parser Char
simpleReg = rangeChar ['a'..'z']


-- QUANTIFICATOR = '+' | '*'
quantifier :: Parser Char
quantifier  = char '+' +++ char '*'


-- BRACES_REG    = '(' BASE_REG ')' QUANTIFICATOR
bracesReg :: Parser String -> Parser String
bracesReg reg = do
                 st  <- char '(' -- check left bracket
                 xs  <- reg     -- inside bracket should be nonempty regular expression
                 end <- char ')' -- check right bracket
                 q   <- quantifier -- after closing bracket should be quantifier ('+' or '*')
                 return ([st]++xs++[end]++[q]) -- save parsed result string



-- ******** META PARSER COMBINATORS  **********

-- TEST
metaSimpleReg :: Parser (Parser Char)
metaSimpleReg = do
                 c <- rangeChar ['a'..'z']
                 return (char c) -- <- create parser that consumes one char. of value 'c'


metaConcatReg :: Parser (Parser String)
metaConcatReg = do
                 str <- many1 simpleReg
                 return (string str) -- < create parser that consumes string of value 'str'


metaStartReg :: Parser (Parser String)
metaStartReg = do           
             parseRes <- metaBaseReg
             empty -- after the entire parsing process
                   -- input string should be empty
             return (do 
                       a <- parseRes
                       empty
                       return a)


metaBaseReg :: Parser (Parser String)
metaBaseReg = do 
               parseCmp  <- metaCompoundReg
               parsePlus <- metaPlusReg

               case parsePlus of 
                 [plus] -> return ( parseCmp `mplus` plus) -- SOULD BE `mplus` instead of  `+++` !!!!!
                 []     -> return (parseCmp)


metaPlusReg :: Parser [Parser String]
metaPlusReg = zeroOne (do a <- char '+'
                          parserBase <- metaBaseReg
                          return parserBase)


metaCompoundReg :: Parser (Parser String)
metaCompoundReg = 
                  do
                    parseBrac <- metaBracesReg
                    parseComp <- zeroOne (do {parserComp <- metaCompoundReg;
                                              return parserComp})
                    case parseComp of
                        [comp] -> return (do
                                            a <- parseBrac
                                            b <- comp
                                            return (a++b))
                        []     -> return (parseBrac)
                  +++
                  do
                    parseConc <- metaConcatReg
                    parseComp <- metaCompoundReg

                    return (do {a <- parseConc; 
                                b <- parseComp; 
                                return (a++b)})
                  +++
                  do
                    parseConc <- metaConcatReg
                    return parseConc


metaBracesReg :: Parser (Parser String)
metaBracesReg = do
              st  <- char '(' -- 
              parseExpr  <- metaBaseReg     -- 
              end <- char ')' -- 
              quan  <- quantifier -- 
              --return (st:xs++[end]++[q]) -- save parsed result string
              if quan == '*'
                then 
                  return (do
                            a <- many parseExpr
                            return (concat a))
              else -- '+'
                  return (do
                            a <- many1 parseExpr
                            return (concat a) )

---- *****************************************************************************************

--testParser1 = string "abc"
--testParser2 = char '2'
--testParser3 = many1 $ char 'a'
--testParser4 = many $ rangeChar ['a'..'d']

testMetaParser1 = metaConcatReg
testMetaParser2 = metaStartReg

main :: IO ()
main = do
         putStrLn "Test line! :)"
         --print $ parse startReg "a+(b)*+o"
         --start <- getCurrentTime

         print $ parse testSmall "bgg"

         --let fin_parser = parse testMetaParser2 "petya(z+d+j)*cc"
         --case fin_parser of
         --   [] -> putStrLn "invalid regular expression pattern..." 
         --   [(p,_)] -> print $ parse p "petyacc"
         --   _ -> putStrLn "More parsers have been returned... WTF??"  

         --stop <- getCurrentTime
         --print $ diffUTCTime stop start