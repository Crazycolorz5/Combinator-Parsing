
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
