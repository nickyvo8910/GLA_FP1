module Main (main) where
import Items
-- import Receipts
import System.IO
--  ------------------------------------------------------------------- [ Main ]

main :: IO ()
main = do
  let aliceReceipt = ""
  let bobReceipt = ""
  let charlieReceipt = ""
  generateReceipt aliceReceipt
  generateReceipt bobReceipt
  generateReceipt charlieReceipt


generateReceipt :: String -> IO ()
generateReceipt r = do
  putStrLn "<start>"
  putStr r
  putStrLn "<end>"

--  -------------------------------------------------------------------- [ EOF ]

 -- Alice Input
 appleFP = Items Apple 0.50 NA
 WatermelonFP = Items Watermelon 3.00 NA
 CoffeeFP = Items Cofee 6.00 NA
 LovageFP = Items Lovage 2.59 NA
 CheeseFP =  Items Cheese 3.00 NA
 CreamCheeseFP = Items CreamCheese 2.50 NA 