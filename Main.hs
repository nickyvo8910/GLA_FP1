module Main (main) where
  import Alice
  import Bob
  import Charlie
  import Items
  import Receipts
  --  ------------------------------------------------------------------- [ Main ]
  
  main :: IO ()
  main = do
    let aliceReceipt = toString (checkout aliceItems)
    let bobReceipt = toString (checkout bobItems)
    let charlieReceipt = toString (checkout charlieItems)
    generateReceipt aliceReceipt
    generateReceipt bobReceipt
    generateReceipt charlieReceipt


  generateReceipt :: String -> IO ()
  generateReceipt r = do
    putStrLn "<start>"
    putStr r
    putStrLn "<end>"

  --  -------------------------------------------------------------------- [ EOF ]
