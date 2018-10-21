module Ops.Flags where
import Data.List
import System.Console.GetOpt
import System.Exit
import System.IO

data Flag
    = Output String         -- -o
    | Debug                 -- -d
    | Libs String           -- -I
    | Help                  -- -h, --help
    deriving (Eq,Ord,Show)

flags =
   [Option ['o'] []       (ReqArg Output "FILE")
        "Redirect output to specified file."
   ,Option ['d'] []       (NoArg Debug)
        "Does not generate C++ output but prints the final IR as Json."
   ,Option ['I'] []       (ReqArg Libs "DIR")
        "Where to look for Coq ADT typeclasses and instances. Default is `classes`"
   ,Option []    ["help"] (NoArg Help)
        "Print this help message"
   ,Option ['h'] []       (NoArg Help)
        "Print this help message"
   ]

-- Take the output of getArgs and return (flags, filenames)
getFlags :: [String] -> IO ([Flag], String)
getFlags argv = case getOpt Permute flags argv of
    (args,fs,[]) -> do
        let files = if null fs then ["-"] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitSuccess
            else return (nub args, head files)
    (_,_,errs)      -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
    where header = "Usage: machcoq [-d] [-o cpp_file] <json_file ...>"
