module Week11.SExpr where

  import Control.Applicative
  import Data.Char (isAlpha, isAlphaNum, isSpace)
  import Week11.AParser (Parser, char, posInt, satisfy)
  
  ------------------------------------------------------------
  --  Exercise 1: Parsing repetitions
  ------------------------------------------------------------
  -- oneOrMore p :: Parser [a]
  -- (<|>) :: Parser [a] -> Parser [a] -> Parser [a]
  -- pure [] :: Parser [a]
  zeroOrMore :: Parser a -> Parser [a]
  zeroOrMore p = oneOrMore p <|> pure []
  
  -- (:) :: a -> [a] -> [a]
  -- (:) <$> f a :: f ([a] -> [a])
  -- (:) <$> f a <*> f a :: f [a]
  oneOrMore :: Parser a -> Parser [a]
  oneOrMore p = (:) <$> p <*> zeroOrMore p
  
  ------------------------------------------------------------
  --  Exercise 2: Utilities
  ------------------------------------------------------------
  -- satisfy isSpace :: Parser Char
  -- zeroOrMore :: Parser Char -> Parser [Char]
  spaces :: Parser String
  spaces = zeroOrMore (satisfy isSpace)
  
  -- (:) :: Char -> [Char] -> [Char]
  -- (:) <$> f Char :: f ([Char] -> [Char])
  -- (:) <$> f Char <*> f [Char] :: f [Char]
  ident :: Parser String
  ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)
  
  ------------------------------------------------------------
  --  Exercise 3: Parsing S-expressions
  ------------------------------------------------------------
  -- An "identifier" is represented as just a String; however, only
  -- those Strings consisting of a letter followed by any number of
  -- letters and digits are valid identifiers.
  type Ident = String
  
  -- An "atom" is either an integer value or an identifier.
  data Atom
    = N Integer
    | I Ident
    deriving (Show, Eq)
  
  -- An S-expression is either an atom, or a list of S-expressions.
  data SExpr
    = A Atom
    | Comb [SExpr]
    deriving (Show, Eq)
  
  -- N :: Integer -> Atom, I :: Ident -> Atom
  -- posInt :: Parser Integer, ident :: Parser String
  parseAtom :: Parser Atom
  parseAtom = N <$> posInt <|> I <$> ident
  
  parseComb :: Parser [SExpr]
  parseComb = char '(' *> oneOrMore parseSExpr <* char ')'
  
  -- A :: Atom -> SExpr, Comb :: [SExpr] -> SExpr
  parseSExpr :: Parser SExpr
  parseSExpr = spaces *> (A <$> parseAtom <|> Comb <$> parseComb) <* spaces