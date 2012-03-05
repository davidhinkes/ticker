{-# LANGUAGE OverloadedStrings #-}

import Ticker
import Ticker.Investors

import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.CaseInsensitive as CI
import Data.Enumerator (run_, enumList, ($$))
import Data.Monoid
import qualified Data.Text as DT
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import System.Random

mkInvestors =
  let rs = mkRandomInvestors (mkStdGen 3) (1.0/10.0)
      hs = mkHinkesInvestors 
  in (take 0 hs) ++ (take 10 rs)

staticFileHandler fileName = ResponseFile status200
                                          [ (CI.mk . B.pack $ "Content-Type", B.pack "text/html") ]
                                          fileName
                                          Nothing

webapp request = return $ case pathInfo request of
  [ "process.csv" ] -> let headers = [ (CI.mk . B.pack $ "Content-Type", B.pack "text/csv") ]
                           market = mkMarket mkInvestors (Ballance 1000.0 100)
                           ticker = Ticker Nothing Nothing 10 0
                           ts = take 2000 $ runSimulation 0 ticker market
                           output =  (formatTickers ts)
                       in ResponseBuilder status200 headers (mconcat (map copyByteString output))
  _ -> staticFileHandler "index.html"

main :: IO ()
main = do
  let ballance = Ballance 1000.0 100
  let port = 8080
  Network.Wai.Handler.Warp.run port webapp

formatTickers ts = B.pack "Ticker,Price\n" : (formatTickers' 0 ts) where
  formatTickers' _ [] = []
  formatTickers' n (t:ts) = B.pack ((show n) ++ (",") ++
                                    (show . Ticker.last $ t) ++
                                    --(show . Ticker.volume $ t) ++
                                    "\n") : formatTickers' (n+1) ts

runSimulation :: Int -> Ticker -> Market -> [Ticker]
runSimulation ticksPerDay ticker market = runSimulation' 0 ticker market where 
  runSimulation' i t m =
    let m' = if i == 0 then cleanMarketOrders m else m
        (t', m'') = runOnce t m'
        i' = if i > ticksPerDay then 0 else i+1
    in t' : runSimulation' i' t' m''
