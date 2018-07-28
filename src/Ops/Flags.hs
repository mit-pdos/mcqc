module Ops.Flags where
import Control.Monad
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

data Flag
    = Output                -- -o
    | Debug                 -- -d
    | Libs                  -- -s
    | Help                  -- -h, --help
    deriving (Eq,Ord,Enum,Show,Bounded)

flags =
   [Option ['o'] []       (NoArg Output)
        "Redirect output to specified file."
   ,Option ['d'] []       (NoArg Debug)
        "Does not generate C++ output but prints the final IR as Json."
   ,Option ['s'] []       (NoArg Libs)
        "Print functions found by libclang in ./include/."
   ,Option []    ["help"] (NoArg Help)
        "Print this help message"
   ,Option ['h'] []       (NoArg Help)
        "Print this help message"
   ]

getFlags argv = case getOpt Permute flags argv of
    (args,fs,[]) -> do
        let files = if null fs then ["-"] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitWith ExitSuccess
            else return (nub args, files)

    (_,_,errs)      -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where header = "Usage: machcoq [-ds] [-o cpp  file] [json file ...]"
