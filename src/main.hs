import Ticker
import Ticker.Investors

import System.Random
import Data.Map

main :: IO ()
main = do
  let rs = mkRandomInvestors (mkStdGen 3) (1.0/10.0)
  let hs = mkHinkesInvestors
  --let bs = mkBallancedInvestors (mkStdGen 9)
  let ballance = Ballance 1000.0 100
  let market = mkMarket (take 10 hs ++ take 100 rs) ballance
  let ticker = Ticker Nothing Nothing 10 0
  
  --putStrLn "Ticker.  David Hinkes. 2011."
  run 0 ticker market
  --let runs = take 1000 $ runDays ticker market
  --putStrLn $ unlines $ Prelude.map (\((Ticker _ _ v _), _) -> show v) $ runs
  --let (_, m) = Prelude.last runs
  --let (_, hinkesInvestorBallance, _) = m ! 0
  --putStrLn $ show $ hinkesInvestorBallance

run :: Int -> Ticker -> Market -> IO ()
run n t m = do
  let m' = if n == 0 then cleanMarketOrders m else m
  let (t',m'') = runOnce t m'
  putStrLn $ show $ Ticker.last t'
  let n' = if n > 25 then 0 else n+1
  run n' t' m''
