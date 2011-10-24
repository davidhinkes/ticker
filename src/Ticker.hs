-- David Hinkes.  2011.
-- Ticker framework for market simulation.

module Ticker where

import Data.List
import Data.Map
import System.Random

data Ballance = Ballance {
  dollars :: Float,
  shares ::  Int
} deriving (Show)

type Price = Float
type Bid = Maybe Price
type Ask = Maybe Price
type Last = Price
type Volume = Int
type InvestorID = Int

data Ticker = Ticker { bid :: Bid, ask ::  Ask, last :: Last, volume :: Volume }
  deriving Show

data Side = Buy | Sell
  deriving Show

data Order = Limit {
  side ::  Side,
  price :: Price
} deriving Show

data Investor = Investor {
  invest :: Ballance -> Ticker -> [Order] -> (Investor, [Order])
}

type Market = Map InvestorID (Investor, Ballance, [Order])

mkMarket :: [Investor] -> Ballance -> Market
mkMarket investors bal = Data.Map.fromList $ mkMarket' investors bal 0 where
  mkMarket' (i:is) b iid = (iid, (i, b, [])):mkMarket' is b (iid+1)
  mkMarket' [] _ _ = []

-- Prune orders w/o adequate ballance.
validateOrders :: Ballance -> [Order] -> [Order]
validateOrders ballance orders = validateOrders' ballance orders 0.0 0 where
  validateOrders' b (o@(Limit Buy p):os) dollarsCommitted sharesCommitted =
    if dollars b >= p + dollarsCommitted
    then o:validateOrders' b os (dollarsCommitted + p) sharesCommitted
    else validateOrders' b os dollarsCommitted sharesCommitted
  validateOrders' b (o@(Limit Sell _):os) dollarsCommitted sharesCommitted =
    if shares b >= (sharesCommitted + 1)
    then o:validateOrders' b os dollarsCommitted (sharesCommitted+1)
    else validateOrders' b os dollarsCommitted sharesCommitted
  validateOrders' _ [] _ _ = []

data AuctionEntry = AuctionEntry { investorID :: InvestorID, order :: Order }

auctionEntryPrice :: AuctionEntry -> Price
auctionEntryPrice (AuctionEntry _ o) = price o

instance Eq AuctionEntry where
  a == b = auctionEntryPrice a == auctionEntryPrice b

instance Ord AuctionEntry where
  compare a b = compare (auctionEntryPrice a) (auctionEntryPrice b)

mkAuctionEntries :: InvestorID -> [Order] -> [AuctionEntry]
mkAuctionEntries iid (o:os) = (AuctionEntry iid o):mkAuctionEntries iid os
mkAuctionEntries _ [] = []

updateMarketOrders :: Ticker -> Market -> Market
updateMarketOrders ticker m = Data.Map.map f m where
  f :: (Investor, Ballance, [Order]) -> (Investor, Ballance, [Order])
  f (i, b, os) = let (i', os') = invest i b ticker os
                 in (i', b, validateOrders b os')

extractAuctionEntries :: Market -> [AuctionEntry]
extractAuctionEntries m = foldWithKey f [] m where
  f iid (_, _, os) entries = entries ++ (mkAuctionEntries iid os)

runAuction :: Ticker -> [AuctionEntry] -> [AuctionEntry] -> (Ticker, [AuctionEntry], [AuctionEntry])
runAuction oldTicker buys sells = let
  buys' = reverse . sort $ buys
  sells' = sort sells
  transactions ns = quot (length ns) 2
  leftOvers ps bs ss = let n = transactions ps in drop n bs ++ drop n ss
  runAuction' (b:bs) (s:ss) p pairs =
    if b >= s
    then let avePrice = (auctionEntryPrice s + auctionEntryPrice b) / 2.0
         in runAuction' bs ss avePrice (b:s:pairs)
    else (Ticker (Just $ price . order $ b) (Just $ price . order $ s) p $ transactions pairs, pairs, leftOvers pairs buys' sells')
  runAuction' [] _ p pairs = (Ticker Nothing Nothing p $ transactions pairs, pairs, leftOvers pairs buys' sells')
  runAuction' _ [] p pairs = (Ticker Nothing Nothing p $ transactions pairs, pairs, leftOvers pairs buys' sells')
  in runAuction' buys' sells' (Ticker.last oldTicker) []

cleanMarketOrders :: Market -> Market
cleanMarketOrders m = Data.Map.map (\(i, b, _) -> (i, b, [])) m

updateMarketLeftOver :: Market -> [AuctionEntry] -> Market
updateMarketLeftOver m (ae:aes) = updateMarketLeftOver (adjust (f $ order ae) (investorID ae) m) aes where
   f newOrder (i, b, orders) = (i, b, newOrder:orders)
updateMarketLeftOver market [] = market

updateMarketBallance :: Market -> Ticker -> [AuctionEntry] -> Market
updateMarketBallance m ticker (ae:aes) = updateMarketBallance (adjust (f $ order ae) (investorID ae) m) ticker aes where
  f (Limit Buy p) (i, (Ballance d s), orders) = (i, Ballance (d - p) (s+1), orders)
  f (Limit Sell p) (i, (Ballance d s), orders) = (i, Ballance (d + p) (s-1), orders)
updateMarketBallance market _ [] = market

runOnce :: Ticker -> Market -> (Ticker, Market)
runOnce ticker market = let market' = updateMarketOrders ticker market
                            auctionEntries = extractAuctionEntries market'
                            buyFilter (AuctionEntry _ (Limit Buy _)) = True
                            buyFilter _ = False
                            sellFilter = not . buyFilter
                            buys = Prelude.filter buyFilter auctionEntries
                            sells = Prelude.filter sellFilter auctionEntries
                            (ticker', pairs, leftovers) = runAuction ticker buys sells
                            market'' = updateMarketBallance market' ticker' pairs
                            market''' = cleanMarketOrders market''
                            market'''' = updateMarketLeftOver market''' leftovers
                        in (ticker', market'''')

runMany :: Ticker -> Market -> [(Ticker, Market)]
runMany t m = let (t', m') = runOnce t m in (t', m'):runMany t' m'

runDay :: Ticker -> Market -> [(Ticker, Market)]
runDay t m = take 100 $ runMany t $ cleanMarketOrders m

runDays :: Ticker -> Market -> [(Ticker, Market)]
runDays t m = let xs = take 25 $ runMany t m
                  (t',m') = Data.List.last xs
                  m'' =  cleanMarketOrders m'
                  in xs ++ runDays t' m''
