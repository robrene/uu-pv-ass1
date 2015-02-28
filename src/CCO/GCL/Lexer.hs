module CCO.GCL.Lexer (
    Token
  , lexer
  , keyword    -- :: String -> Parser Token String
  , name       -- :: Parser Token Name
  , nat        -- :: Parser Token Int
  , str        -- :: Parser Token String
  , spec       -- :: Char -> Parser Token Char
) where

import CCO.GCL.Base      (Name)
import CCO.Lexing hiding (satisfy)
import CCO.Parsing       (Symbol (describe), Parser, satisfy, (<!>))
import Control.Applicative

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

-- | Type of tokens.
data Token
  = Keyword  { fromKeyword :: String  }    -- ^ Keyword.
  | Name     { fromName    :: Name    }    -- ^ Name.
  | Nat      { fromNat     :: Int     }    -- ^ Nat/Int.
  | Str      { fromStr     :: String  }    -- ^ String.
  | Spec     { fromSpec    :: Char    }    -- ^ Special character.

instance Symbol Token where
  describe (Keyword _)  lexeme = "keyword "  ++ lexeme
  describe (Nat _)      lexeme = "integer "  ++ lexeme
  describe (Name _)     lexeme = "name "     ++ lexeme
  describe (Str _)      lexeme = "string \"" ++ lexeme ++ "\""
  describe (Spec _)     lexeme =                lexeme

-- | Retrieves whether a 'Token' is a 'Keyword'.
isKeyword :: Token -> Bool
isKeyword (Keyword _) = True
isKeyword _           = False

-- | Retrieves whether a 'Token' is a 'Nat'.
isNat :: Token -> Bool
isNat (Nat _) = True
isNat _       = False

-- | Retrieves whether a 'Token' is a 'Name'.
isName :: Token -> Bool
isName (Name _) = True
isName _        = False

-- | Retrieves whether a 'Token' is a 'Str'.
isStr :: Token -> Bool
isStr (Str _) = True
isStr _       = False

-- | Retrieves whether a 'Token' is a 'Spec'.
isSpec :: Token -> Bool
isSpec (Spec _) = True
isSpec _        = False

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

-- | A 'Lexer' that recognises (and ignores) whitespace.
layout_ :: Lexer a
layout_ = ignore (some (anyCharFrom " \n\t"))

-- | A 'Lexer' that recognises 'Keyword' tokens.
keyword_ :: Lexer Token
keyword_ = fmap Keyword $ string "skip"
                      <|> string "assert" <|> string "assume"
                      <|> string "return"
                      <|> string "inv" <|> string "while" <|> string "do"
                      <|> string "var" <|> string "in" <|> string "end"
                      <|> string "true" <|> string "false"
                      <|> string "not"
                      <|> string "forall"
                      <|> string "or" <|> string "and"
                      <|> string "int" <|> string "bool"

-- | A 'Lexer' that recognises 'Name' tokens.
name_ :: Lexer Token
name_ = (\c cs -> Name (c:cs)) <$> (alpha <|> char '_') <*> many (alphaNum <|> char '_')

-- | A 'Lexer' that recognises 'Nat' tokens.
nat_ :: Lexer Token
nat_ = (Nat . read) <$> some digit

-- | A 'Lexer' that recognises 'Str' tokens
str_ :: Lexer Token
str_ =  Str <$ char '"' <*> many (anyCharBut "\"") <* char '"'

-- | A 'Lexer' that recognises 'Spec' tokens.
spec_ :: Lexer Token
spec_ = Spec <$> anyCharFrom "{}:=;#[],()+-*/|&<>"

-- | The 'Lexer' for the language.
lexer :: Lexer Token
lexer = layout_ <|> keyword_ <|> name_ <|> nat_ <|> str_ <|> spec_

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | A 'Parser' that recognises a specified keyword.
keyword :: String -> Parser Token String
keyword key = fromKeyword <$>
              satisfy (\tok -> isKeyword tok && fromKeyword tok == key) <!>
              "keyword " ++ key

-- | A 'Parser' that recognises names.
name :: Parser Token Name
name = fromName <$> satisfy isName <!> "name"

-- | A 'Parser' that recognises numbers.
nat :: Parser Token Int
nat = fromNat <$> satisfy isNat <!> "nat"

-- | A 'Parser' that recognises strings.
str :: Parser Token String
str = fromStr <$> satisfy isStr <!> "str"

-- | A 'Parser' that recognises a specified special character.
spec :: Char -> Parser Token Char
spec c = fromSpec <$>
         satisfy (\tok -> isSpec tok && fromSpec tok == c) <!>
         [c]
