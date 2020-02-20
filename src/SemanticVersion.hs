module SemanticVersion where

import Control.Applicative
import Text.Trifecta
-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString =
         NOSS String
       | NOSI Integer
       deriving (Show)
type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]
data SemVer =
       SemVer Major Minor Patch Release Metadata
  deriving (Show)

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  _ <- char '.'
  minor <- decimal
  _ <- char '.'
  patch <- decimal
  release' <- option [] parseRelease
  metadata <- option [] parseMetadata
  return $ SemVer major minor patch release' metadata

parseNumberOrString :: Parser NumberOrString
parseNumberOrString =
  ((NOSI <$> decimal) <|> (NOSS <$> some alphaNum))

parseRelease :: Parser [NumberOrString]
parseRelease = do
  _ <- char '-'
  sepBy1 parseNumberOrString (char '.')

parseMetadata :: Parser [NumberOrString]
parseMetadata = do
  _ <- char '+'
  sepBy1 parseNumberOrString (char '.')
-- Expected results:
-- Prelude> ps = parseString
-- Prelude> psv = ps parseSemVer mempty Prelude> psv "2.1.1"
-- Success (SemVer 2 1 1 [] []) Prelude> psv "1.0.0-x.7.z.92" Success (SemVer 1 0 0
--               [NOSS "x",
--                NOSI 7,
--                NOSS "z",
--                NOSI 92] [])
-- Some slightly more advanced test cases:
-- Prelude> psv "1.0.0-gamma+002" Success (SemVer 1 0 0
-- [NOSS "gamma"] [NOSI 2]) Prelude> psv "1.0.0-beta+oof.sha.41af286" Success (SemVer 1 0 0
--               [NOSS "beta"]
--               [NOSS "oof",
--                NOSS "sha",
-- NOSS "41af286"])

-- CHAPTER24. PARSERCOMBINATORS 1451 And lastly, the correct total ordering of semantic versions:
--      Prelude> big = SemVer 2 1 1 [] []
--      Prelude> little = SemVer 2 1 0 [] []
--      Prelude> big > little
--      True
