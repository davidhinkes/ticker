{-# LANGUAGE OverloadedStrings #-}

import Ticker
import Ticker.Investors

import Blaze.ByteString.Builder (copyByteString)
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.Enumerator (run_, enumList, ($$))
import Data.List
import Data.Monoid
import qualified Data.Text as DT
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import System.Random

mkInvestors =
  let rs = mkRandomInvestors (mkStdGen 3) (2.0/10.0)
      hs = mkHinkesInvestors 
  in (take 20 hs) ++ (take 100 rs)

staticFileHandler fileName = ResponseFile status200
                                          [ (CI.mk . B.pack $ "Content-Type", B.pack "text/html") ]
                                          fileName
                                          Nothing

webapp request = return $ case pathInfo request of
  [ "process.csv" ] -> let headers = [ (CI.mk . B.pack $ "Content-Type", B.pack "text/csv") ]
                           market = mkMarket mkInvestors (Ballance 1000.0 100)
                           ticker = Ticker Nothing Nothing 10 0
                           ts = runSimulation 25 1000 ticker market
                           output =  (formatTickers ts)
                       in ResponseBuilder status200 headers (mconcat (map copyByteString output))
  _ -> staticFileHandler "index.html"

main :: IO ()
main = do
  let port = 8080
  Network.Wai.Handler.Warp.run port webapp

formatTickers ts = B.pack "Ticker,Price\n" : (formatTickers' 0 ts) where
  formatTickers' _ [] = []
  formatTickers' n (t:ts) = B.pack ((show n) ++ (",") ++
                                    (show . Ticker.last $ t) ++
                                    "\n") : formatTickers' (n+1) ts

runSimulation :: Int -> Int -> Ticker -> Market -> [Ticker]
runSimulation ticksPerDay days ticker market =
  concat $ evalState (runDays days ticksPerDay) (ticker,market)
