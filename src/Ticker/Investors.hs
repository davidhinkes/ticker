-- David Hinkes
-- Investors to be used as actors within the framework.

module Ticker.Investors where

import Control.Monad.State
import System.Random
import Ticker

-- Buy and sell a little bit every day @ fixed price.
mkHinkesInvestor :: Investor
mkHinkesInvestor = Investor i () where
  i _ _ _ = return $ (take 100 $ repeat $ Limit Buy 9.0) ++ (take 100 $ repeat $ Limit Sell 11.0)

mkHinkesInvestors :: [Investor]
mkHinkesInvestors = mkHinkesInvestor : mkHinkesInvestors

-- RandomInvestor.  Randomly buy or sell at any given time.
mkRandomInvestor :: RandomGen g => g -> Float -> Investor
mkRandomInvestor g f = Investor invest' g where
  invest' _ (Ticker _ _ lastPrice _) [] = do
    let p = case lastPrice of
              0.0 -> 10
              _ -> lastPrice
    g <- get
    let (randomNumber, g') = randomR (0.0, 1.0) g
    let (priceScale, g'') = randomR (-0.03, 0.03) g'
    put g''
    let orders = case randomNumber of
                   x | x > (1.0 - f) -> [Limit Buy (p*(1.0-priceScale))]
                   x | x < f -> [Limit Sell (p*(1.0+priceScale))]
                   _         -> []
    return orders
  -- If there are already outstanding orders, don't do anything.
  invest' _ _ orders = return orders

mkRandomInvestors :: RandomGen g => g -> Float -> [Investor]
mkRandomInvestors g f = let (g1, g2) = System.Random.split g
                        in mkRandomInvestor g1 f : mkRandomInvestors g2 f

{--
mkBallancedInvestor :: RandomGen g => g -> Float -> Float -> Investor
mkBallancedInvestor g activityRatio assetRatio = Investor invest where
  (g', r) = randomR (0.0, 0.001) g
  i = mkBallancedInvestor g' activityRatio assetRatio
  invest b t [] = let d = dollars b
                      p = Ticker.last t
                      targetShares = floor $ (d*r)/(p*(1-r)) :: Int
                      currentShares = shares b
                      in case compare targetShares currentShares of
                        EQ -> (i, [])
                        GT -> (i, take (targetShares - currentShares) $ repeat (Limit Buy p))
                        LT -> (i, take (currentShares - targetShares) $ repeat (Limit Sell p))
  invest _ _ orders = (i, orders)

mkBallancedInvestors :: RandomGen g => g -> [Investor]
mkBallancedInvestors g = let (r, g') = randomR (0.01, 0.99) g
                             i = mkBallancedInvestor r
                             in i:mkBallancedInvestors g'a
--}
