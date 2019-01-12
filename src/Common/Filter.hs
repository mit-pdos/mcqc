module Common.Filter where
import Common.Config
import Data.Text (Text)
import CIR.Expr
import Types.Context
import qualified Data.Map  as M
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

filterDecl :: Nameful d => Context a -> [d] -> [d]
filterDecl ctx = filter (\d -> not $ getname d `M.member` ctx)
