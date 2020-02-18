{-# LANGUAGE OverloadedStrings #-}
module Text.Fractions where
import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    case denominator of
      0 -> fail "denominator cannot be zero"
      _ -> return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

testVirtuous :: IO ()
testVirtuous = do
    let virtuousFraction' = parseString virtuousFraction mempty
    print $ virtuousFraction' badFraction
    print $ virtuousFraction' alsoBad
    print $ virtuousFraction' shouldWork
    print $ virtuousFraction' shouldAlsoWork
