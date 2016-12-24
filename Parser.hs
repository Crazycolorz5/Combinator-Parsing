module Parser where

import Control.Arrow
import Control.Monad.Fail

newtype Parser source out = Parser { tryParse :: source -> Maybe (source, out) } 
--Model is source is what we're trying to parse an out out of. It might fail (nothing), or give the remainder of source and an out.
--The Parser is just a way to do this parsing, hence the function.

instance Functor (Parser a) where
    fmap f parser = Parser {tryParse = fmap (second f) . tryParse parser}
    
instance Applicative (Parser a) where
    pure val = Parser {tryParse = \a->Just (a, val)}
    pF <*> pVal = Parser { tryParse = \a -> case tryParse pF a of
                                                 Nothing -> Nothing
                                                 Just (aRest, f) -> fmap (second f) (tryParse pVal aRest) }

instance Monad (Parser a) where
    parser >>= pf = Parser { tryParse = \a -> case tryParse parser a of
                                                   Nothing -> Nothing
                                                   Just (aRest, val) -> tryParse (pf val) aRest}
--Parse one way, then try to parse parameterized on the output.
                                                   
instance MonadFail (Parser a) where
    fail _ = Parser { tryParse = const Nothing }
--We can fail parsing by short circuiting.

(<&>)::Parser a b -> Parser a c -> Parser a (b,c)
pb <&> pc = do
    bVal <- pb
    cVal <- pc
    return (bVal, cVal)
infixl 3 <&>
--Parse one thing, then another, then give the result of both.
    
(<|>)::Parser a b -> Parser a c -> Parser a (Either b c)
pb <|> pc = Parser {tryParse = \input -> case tryParse pb input of
                               Nothing -> case tryParse pc input of
                                    Nothing -> Nothing
                                    Just (aRes, cRes) -> Just (aRes, Right cRes)
                               Just (aRes, bRes) -> Just (aRes, Left bRes) }
infixl 2 <|>
--Parse one thing, and if that fails, then try to parse the other thing.

getResult::Maybe (a, b) -> Maybe b
getResult = fmap snd
--Discards remainder of the source.

kleeneStar::Parser a b -> Parser a [b]
kleeneStar parse = flip fmap ((parse <&> kleeneStar parse) <|> emptyParse) (\result -> case result of
    Right _ -> []
    Left (e, es) -> e : es)

emptyParse::Parser a ()
emptyParse = Parser {tryParse = \input -> Just (input, ())}

parseAnyElem::Parser [a] a
parseAnyElem = Parser {tryParse = \input -> case input of
                            (x:xs) -> Just (xs, x)
                            otherwise -> Nothing }
--Convienient primitive.

parseElem::(Eq a) => a -> Parser [a] a
parseElem c = Parser {tryParse = \input -> case input of
                            (x:xs) -> if x == c then Just (xs, x) else Nothing
                            otherwise -> Nothing }
--Parses only for a specific element and fails otherwise.

parseSequence::(Eq a) => [a] -> Parser [a] [a]
parseSequence word = (foldl (\acc e -> acc >> (parseChar e >> emptyParse)) emptyParse word) >> return word

--Above functions type constrained to Char.
parseAnyChar::Parser String Char
parseAnyChar = parseAnyElem

parseChar::Char -> Parser String Char
parseChar = parseElem

parseWord::String -> Parser String String
parseWord = parseSequence

{-
--Failed previous implementation
--I was trying to use arrows to represent parsers as a composition of smaller parsers
--But, it didn't quite work out; <|> is difficult, for example.
--I'm leaving it here because I want to revisit it sometime.

instance Category Parser where
    id = Parser {runParser = \x -> (x, x)}
    f . g = Parser {runParser = \a->let (aAfterG, gRes) = runParser g a in let (bAfterF, fRes) = runParser f gRes in (aAfterG, fRes)} 

instance Arrow Parser where
    arr f = Parser {runParser = \b->(b, f b)}
    first f = Parser {runParser = \(b,d)->let (fb, fc) = runParser f b in ((fb, d),(fc, d))}
-}

--type StringParser = Parser String
{-
parseExpression::StringParser (a, b, c)
parseExpression = proc input -> do
    term <- parseTerm -< input
    addOp <- parseAddOp -< input
    factor <- parseFactor -< input
    returnA -< (term, addOp, factor)

parseTerm::StringParser a
parseTerm = undefined
parseAddOp::StringParser a
parseAddOp = undefined
parseFactor::StringParser a
parseFactor = undefined
-}
