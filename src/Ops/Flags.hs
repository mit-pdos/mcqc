module Ops.Flags where
import Control.Monad
import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf

data Flag
    = Output String         -- -o
    | Debug                 -- -d
    | Libs                  -- -s
    | Help                  -- -h, --help
    deriving (Eq,Ord,Show)

flags =
   [Option ['o'] []       (ReqArg (\arg -> Output arg) "FILE")
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

-- Get output filename if exists
getOutput :: [Flag] -> Maybe String
getOutput [] = Nothing
getOutput ((Output fn):fs) = Just fn
getOutput (f:fs) = getOutput fs

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

    where header = "Usage: machcoq [-ds] [-o cpp_file] [json_file ...]"
