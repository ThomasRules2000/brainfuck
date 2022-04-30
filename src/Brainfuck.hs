module Brainfuck where
import           Command                    (Command(..), Program, Data(..))
import           Control.Monad.State        (gets, modify, evalStateT, liftIO, StateT)
import qualified Data.Text.IO        as TIO
import           Data.Word                  (Word8)
import           Lib                        (coerceEnum)
import           Parser                     (getProgram)
import           System.Environment         (getArgs)

-- Main entrypoint, reads command line arguments to get the file
brainfuck :: IO ()
brainfuck = getArgs >>= \case
    [file] -> do 
        prog <- TIO.readFile file
        evalStateT (runProgram $ getProgram prog) $ Data [] 0 []
    _ -> print "Usage \"stack run filename\""

-- Increment the data pointer
incPointer :: Data -> Data
incPointer (Data prev d []) = Data (d:prev) 0 []
incPointer (Data prev d (n:next)) = Data (d:prev) n next

-- Decrement the data pointer
decPointer :: Data -> Data
decPointer (Data [] _ _) = error "OOM"
decPointer (Data (p:prev) d next) = Data prev p (d:next)

-- Update the byte at the pointer using a function
modifyByte :: (Word8 -> Word8) -> Data -> Data
modifyByte f dat@Data{..} = dat{currByte=f currByte}

runProgram :: Program -> StateT Data IO ()
runProgram [] = return ()
runProgram (p:ps) = do
    case p of
        IncPointer -> modify incPointer
        DecPointer -> modify decPointer
        IncByte    -> modify (modifyByte (+1))
        DecByte    -> modify (modifyByte (subtract 1))
        Output     -> do
            dat <- gets currByte
            liftIO $ putChar $ coerceEnum dat
        Input      -> do
            inp <- coerceEnum <$> liftIO getChar
            modify (modifyByte $ const inp)
        Loop inner -> go
            where go = gets currByte >>= \case
                    0 -> return ()
                    _ -> runProgram inner >> go
    runProgram ps
