module Common.Filter where
import Common.Config
import Data.Text (Text)
import qualified Data.Text as T

modpred :: String -> Bool
modpred ('M':t)
    | (T.toLower . T.pack $ t) `elem` libs = False
    | otherwise = True
modpred t
    | (T.toLower . T.pack $ t) `elem` base = False
    | otherwise = True

filterMod :: [Text] -> [Text]
filterMod = map T.pack . filter modpred . map T.unpack

-- Get allowed includes based on the libraries in the config
filterAllowed :: [Text] -> [Text]
filterAllowed = filter (`elem` libs)


