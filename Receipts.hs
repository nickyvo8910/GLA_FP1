module Receipts (Receipts(..),checkout)
where
    import Items
    import Data.List
    import Data.Decimal

    data Receipts = Receipts{  
        scannedProducts :: [Items],
        netPrice :: Decimal
    }deriving (Show, Eq, Ord)

    -- checkout
    checkout :: Receipt -> Receipt

    -- scanNew
    scanNew ::  [Items] -> Receipt

    -- checkOffer 0 NA -1 BOGOF else Reduced
    checkOffer :: Items -> Decimal
    -- applyOffer
   
    -- totalPrice
    totalPrice :: [Items] -> Decimal

    --exportReciept


   -- Alice Input
    appleFP = (Items "Apple" 0.50 NA)
    watermelonFP = (Items "Watermelon" 3.00 NA)
    coffeeFP = (Items "Coffee" 6.00 NA)
    lovageFP = (Items "Lovage" 2.59 NA)
    cheeseFP =  (Items "Cheese" 3.00 NA)
    creamCheeseFP = (Items "CreamCheese" 2.50 NA)
    aliceReceipt = [appleFP,watermelonFP,coffeeFP,lovageFP,cheeseFP,creamCheeseFP]

