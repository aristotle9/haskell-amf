import Data.AMF3
import System.Environment
import System.IO
import qualified Data.ByteString as B

main = do
    name:_ <- getArgs
    handle <- openFile name ReadMode
    contents <- B.hGetContents handle
    let ret = parseSOL contents
    case ret of
        Right jsvalue -> putStrLn $ encode jsvalue
        Left message -> putStrLn $ "ParseError: " ++ message
