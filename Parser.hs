module Parser where

import Control.Arrow (first, second)
import Control.Monad.Fail (MonadFail, fail)
import Control.Monad (MonadPlus, mzero, mplus, guard)
import Control.Applicative (Alternative)
import qualified Control.Applicative as A (empty, (<|>))

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
                                   
instance MonadPlus (Parser a) where
    mzero = Parser { tryParse = const Nothing }
    mplus = (<|>) --Note that this is associative, but not commutative (it takes the first left to right that succeeds).
--We can fail parsing by short circuiting to Nothing. mzero (and thus guard) will set it to this.
    
instance Alternative (Parser a) where
    (<|>) = mplus
    empty = mzero
--https://ghc.haskell.org/trac/ghc/wiki/Migration/7.10#GHCsaysNoinstanceforAlternative...

instance MonadFail (Parser a) where
    fail _ = mzero
--See above.

(<&>)::Parser a b -> Parser a c -> Parser a (b,c)
pb <&> pc = do
    bVal <- pb
    cVal <- pc
    return (bVal, cVal)
infixl 3 <&>
--Parse one thing, then another, then give the result of both.
        
(<|>)::Parser a b -> Parser a b -> Parser a b 
pb <|> pc = Parser {tryParse = \input -> case tryParse pb input of
                               Nothing -> case tryParse pc input of
                                    Nothing -> Nothing
                                    Just (aRes, cRes) -> Just (aRes, cRes)
                               Just (aRes, bRes) -> Just (aRes, bRes) }
infixl 2 <|>
--Like <|> but for two parsers of the same type.

parseEither::Parser a b -> Parser a c -> Parser a (Either b c)
parseEither pb pc = Parser {tryParse = \input -> case tryParse pb input of
                               Nothing -> case tryParse pc input of
                                    Nothing -> Nothing
                                    Just (aRes, cRes) -> Just (aRes, Right cRes)
                               Just (aRes, bRes) -> Just (aRes, Left bRes) }
--Parse one thing, and if that fails, then try to parse the other thing.

getResult::Maybe (a, b) -> Maybe b
getResult = fmap snd
--Discards remainder of the source.

kleeneStarThen :: Parser a b -> Parser a c -> Parser a ([b], c)
kleeneStarThen parse1 parse2 = fmap (either (\(res1, (resList, end)) -> (res1:resList, end)) (\x->([], x))) ((parse1 <&> kleeneStarThen parse1 parse2) `parseEither` parse2)
--Turns a parser into a parser for the kleene star of the original expression (with the result in a list), followed by the given parser.

kleeneStar::Parser a b -> Parser a [b]
kleeneStar = fmap fst . flip kleeneStarThen emptyParse
--A simple greedy kleeneStar that takes as many instances of the parse as possible.

optional::Parser a b -> Parser a (Maybe b)
optional p = fmap (either Just (const Nothing)) $ p `parseEither` emptyParse
--Functionally equivalent to a grammar of p | epsilon. Wraps the result in a Maybe.

emptyParse::Parser a ()
emptyParse = Parser {tryParse = \input -> Just (input, ())}
--Takes nothing from the input, and returns unit.

parseAnyElem::Parser [a] a
parseAnyElem = Parser {tryParse = \input -> case input of
                            (x:xs) -> Just (xs, x)
                            otherwise -> Nothing }
--Convienient primitive.

parseElem::(Eq a) => a -> Parser [a] a
parseElem c = do
    e <- parseAnyElem
    guard (e==c)
    return e
--Parses only for a specific element and fails otherwise.

parseSequence::(Eq a) => [a] -> Parser [a] [a]
parseSequence word = (foldl (\acc e -> acc >> (parseElem e >> emptyParse)) emptyParse word) >> return word

--Above functions type constrained to Char.
parseAnyChar::Parser String Char
parseAnyChar = parseAnyElem

parseChar::Char -> Parser String Char
parseChar = parseElem

parseWord::String -> Parser String String
parseWord = parseSequence

