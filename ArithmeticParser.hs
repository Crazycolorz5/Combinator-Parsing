module ArithmeticParser (parse, evaluate) where

import Parser
import Control.Monad.Fail
import Prelude hiding (fail)

data Expression = Expression    (Either     (Term, SumOp, Expression)   Term)
data Term       = Term          (Either     (Factor, MulOp, Term)       Factor)
data Factor     = Factor        (Either     Expression                  Number)
data Number     = Number        (Either     (Digit, Number)             Digit)
newtype Digit   = Digit         Int

data SumOp = Add | Subtract
data MulOp = Multiply | Divide

parse::String -> Maybe Expression
parse = getResult . tryParse parseExpression

evaluate::Expression -> Rational
evaluate = evaluateE

flatTup ((a,b), c) = (a, b, c)

parseExpression::Parser String Expression
parseExpression = fmap Expression $ fmap flatTup (parseTerm <&> parseSumOp <&> parseExpression) <|> parseTerm

parseTerm::Parser String Term
parseTerm = fmap Term $ fmap flatTup (parseFactor <&> parseMulOp <&> parseTerm) <|> parseFactor

parseFactor::Parser String Factor
parseFactor = fmap Factor $ (do
    parseChar '('
    e <- parseExpression
    parseChar ')'
    return e) <|> parseNumber

parseNumber::Parser String Number
parseNumber = fmap Number $ parseDigit <&> parseNumber <|> parseDigit

parseDigit::Parser String Digit
parseDigit = parseAnyChar >>= \c -> case c of 
        '0' -> return $ Digit 0
        '1' -> return $ Digit 1
        '2' -> return $ Digit 2
        '3' -> return $ Digit 3
        '4' -> return $ Digit 4
        '5' -> return $ Digit 5
        '6' -> return $ Digit 6
        '7' -> return $ Digit 7
        '8' -> return $ Digit 8
        '9' -> return $ Digit 9
        otherwise -> fail ""

parseSumOp::Parser String SumOp
parseSumOp = parseAnyChar >>= \c -> case c of 
        '+' -> return Add
        '-' -> return Subtract
        otherwise -> fail ""

parseMulOp::Parser String MulOp
parseMulOp = parseAnyChar >>= \c -> case c of 
        '*' -> return Multiply
        '/' -> return Divide
        otherwise -> fail ""

evaluateE::Expression -> Rational
evaluateE (Expression (Left (t, op, e))) = let g = case op of Add -> (+); Subtract -> (-) in
    g (evaluateT t) (evaluateE e)
evaluateE (Expression (Right t)) = evaluateT t
    
evaluateT::Term -> Rational
evaluateT (Term (Left (f, op, t))) = let g = case op of Multiply -> (*); Divide -> (/) in
    g (evaluateF f) (evaluateT t)
evaluateT (Term (Right f)) = evaluateF f

evaluateF::Factor -> Rational
evaluateF (Factor (Left e)) = evaluateE e
evaluateF (Factor (Right n)) = evaluateN n

evaluateN::Number -> Rational
evaluateN (Number (Left (d, n))) = evaluateN_rec n (evaluateD d)
evaluateN (Number (Right d)) = evaluateD d

evaluateN_rec (Number (Left (d, n))) acc = evaluateN_rec n (10*acc + evaluateD d)
evaluateN_rec (Number (Right d)) acc = acc*10 + evaluateD d

evaluateD::Digit -> Rational
evaluateD (Digit d) = fromIntegral d

instance Show Expression where
    show (Expression (Left (e, op, t))) = show e ++ show op ++ show t
    show (Expression (Right t)) = show t
instance Show Term where
    show (Term (Left (t, op, f))) = show t ++ show op ++ show f
    show (Term (Right f)) = show f
instance Show Factor where
    show (Factor (Left e)) = '(':show e ++ ")"
    show (Factor (Right lit)) = show lit
instance Show Number where
    show (Number (Left (d, n))) = show d ++ show n
    show (Number (Right d)) = show d
instance Show Digit where
    show (Digit a) = show a
instance Show SumOp where
    show Add = "+"
    show Subtract = "-"
instance Show MulOp where
    show Multiply = "*"
    show Divide = "/"


testAddExp = Left ((Right (Right (Right 5))), Add, (Right (Right 6))) -- 5+6
