
import System.Environment.FindBin
import System.Environment


main = do
    withArgs [""] $ return ()
    putStrLn =<< getProgPath
