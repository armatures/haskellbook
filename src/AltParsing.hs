module AltParsing where
import Control.Applicative
import Text.Trifecta
import Text.Fractions

type DecimalOrFraction =
  Either Rational Integer

frac = "1/2"
dec = "123"

parseNos :: Parser DecimalOrFraction
parseNos =
  (try $ Left <$> virtuousFraction)
  <|> (Right <$> parseDecimal)

parseDecimal :: Parser Integer
parseDecimal = decimal

main' = do
  let p f i = parseString f mempty i
  print $ p (parseNos) frac
  print $ p (parseNos) dec
