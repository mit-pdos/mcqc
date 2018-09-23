module Ops.Flags where
import Data.List
import System.Console.GetOpt
import System.Exit
import System.IO

data Flag
    = Output String         -- -o
    | Debug                 -- -d
    | Libs                  -- -s
    | Help                  -- -h, --help
    deriving (Eq,Ord,Show)

flags =
   [Option ['o'] []       (ReqArg Output "FILE")
        "Redirect output to specified file."
   ,Option ['d'] []       (NoArg Debug)
        "Does not generate C++ output but prints the final IR as Json."
   ,Option []    ["help"] (NoArg Help)
        "Print this help message"
   ,Option ['h'] []       (NoArg Help)
        "Print this help message"
   ]

-- Get output filename if exists
getOutput :: [Flag] -> Maybe String
getOutput [] = Nothing
getOutput (Output fn:fs) = Just fn
getOutput (f:fs) = getOutput fs

getFlags argv = case getOpt Permute flags argv of
    (args,fs,[]) -> do
        let files = if null fs then ["-"] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitSuccess
            else return (nub args, files)

    (_,_,errs)      -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)

    where header = "Usage: machcoq [-d] [-o cpp_file] <json_file ...>"
