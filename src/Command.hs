module Command where
import           Data.Word (Word8)

data Command = IncPointer
             | DecPointer
             | IncByte
             | DecByte
             | Output
             | Input
             | Loop Program
             deriving (Eq, Ord, Show)

type Program = [Command]

data Data = Data {
    prevBytes :: [Word8],
    currByte  :: Word8,
    nextBytes :: [Word8]
    } deriving Show
