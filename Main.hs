module Main (main) where

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
