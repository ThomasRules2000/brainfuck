module Parser (getProgram) where
import           Command                         (Command(..), Program)
import           Control.Applicative             (many)
import           Control.Applicative.Combinators (between)
import           Data.Attoparsec.Text            (choice, char, parse, skip, IResult(..), Parser, Result )
import           Data.Maybe                      (catMaybes)
import           Data.Text                       (Text)

parser :: Parser Program
parser = catMaybes <$> many (choice [
        Just IncPointer <$  char '>',
        Just DecPointer <$  char '<',
        Just IncByte    <$  char '+',
        Just DecByte    <$  char '-',
        Just Output     <$  char '.',
        Just Input      <$  char ',',
        Just . Loop     <$> between (char '[') (char ']') parser, -- Loops are programs between square brackets
        Nothing         <$ skip (`notElem` ("<[.+-,]>" :: String)) -- Skip non brainfuck chars (type needed due to OverloadedStrings)
    ])

-- Give the parser no more input until it decides to spit out a result
getResult :: Result a -> a
getResult (Done _ res) = res
getResult (Partial f) = getResult $ f ""
getResult _ = error "Invalid program"

getProgram :: Text -> Program
getProgram = getResult . parse parser
