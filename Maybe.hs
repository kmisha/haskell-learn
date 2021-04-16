import Data.Char

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
     deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken num
  | all isDigit num = Just (Number (read num))
  | otherwise       = Nothing

tokenize :: String -> Maybe [Token]
tokenize = sequence . (map asToken) . words
