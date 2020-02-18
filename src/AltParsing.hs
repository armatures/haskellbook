module AltParsing where
import Control.Applicative
import Text.Trifecta
import Text.Fractions

type DecimalOrFraction =
  Either Rational Float

frac = "1/2"
dec = "1.23"

parseNos :: Parser DecimalOrFraction
parseNos =
  (try $ Left <$> virtuousFraction)
  <|> (Right <$> parseDecimal)

parseDecimal :: Parser Float
parseDecimal = do
  a <- (token decimal)
  char '.'
  b <- (token decimal)
  -- how do we divide this by ten the right number of times?
  return $ fromIntegral a + toDecimal (fromIntegral b)
  
toDecimal :: Float -> Float
toDecimal i =
  if i < 1 then i
                else toDecimal (i/10)
main' = do
  let p f i = parseString f mempty i
  print $ p (parseNos) frac
  print $ p (parseNos) dec
