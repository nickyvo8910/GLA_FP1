module Main (main) where
import Items
import Receipts
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

 