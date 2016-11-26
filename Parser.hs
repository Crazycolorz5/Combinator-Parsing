module Parser where

import Control.Arrow
import Control.Monad.Fail

newtype Parser a b = Parser { tryParse :: a -> Maybe (a, b) }

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

instance MonadFail (Parser a) where
    fail _ = Parser { tryParse = const Nothing }

(<&>)::Parser a b -> Parser a c -> Parser a (b,c)
pb <&> pc = do
    bVal <- pb
    cVal <- pc
    return (bVal, cVal)
infixl 3 <&>
    
(<|>)::Parser a b -> Parser a c -> Parser a (Either b c)
pb <|> pc = Parser {tryParse = \input -> case tryParse pb input of
                               Nothing -> case tryParse pc input of
                                    Nothing -> Nothing
                                    Just (aRes, cRes) -> Just (aRes, Right cRes)
                               Just (aRes, bRes) -> Just (aRes, Left bRes) }
infixl 2 <|>


getResult::Maybe (a, b) -> Maybe b
getResult = fmap snd

parseAnyChar::Parser String Char
parseAnyChar = Parser {tryParse = \input -> case input of
                            (x:xs) -> Just (xs, x)
                            otherwise -> Nothing }

{-
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
