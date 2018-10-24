module Receipt (Receipt(..),Purchase(..),mkPurchase,mkIndexList)
where
    import Items
    import Data.List
    import Numeric
    -- import Data.Decimal
    -- showFFloat (Just 2) theFloat  ""

    data Receipt = Receipt{
        purchasedItems :: [Purchase],
        -- offerApplied :: [Offer],
        -- reducedItems :: [Reduced],
        netPrice :: Float,
        offerSaving ::Float,
        reduceSaving ::Float,
        totalPrice :: Float
    }deriving (Show)

    data Purchase = Purchase{
      itemIndex :: Int,
      itemName :: String,
      itemPrice ::Float
    }deriving (Show)

    -- data Offer :: (Int,Int,Float,Float)
    -- data Reduced :: (Int,FLoat,Float)


    -- checkout
    -- checkout :: Receipt -> Receipt

    -- -- scanNew
    -- mkReceipt ::  [Items] ->Receipt
    -- mkReceipt inputList =
    --   where oldList = scannedProducts thisReceipt
    --
    -- scanNewItem ::  Items ->Receipt -> [Items]
    -- scanNewItem inputItem thisReceipt = insert inputItem oldList
    --   where oldList = scannedProducts thisReceipt
    mkPurchase :: Item ->Int -> Purchase
    mkPurchase inputItem index = Purchase{
    itemIndex = index,
    itemName = itName inputItem,
    itemPrice = itNetPrice inputItem}


  -- mkPurchaseList :: [Item] -> [Purchase]
    mkIndexList :: [Item] -> [(Int,Item)]
    mkIndexList inputList= zip [1..] inputList




    -- checkOffer 0 NA -1 BOGOF else Reduced
    -- checkOffer :: Items -> Decimal
    -- applyOffer

    -- totalPrice
    -- totalPrice :: Receipts -> Float
    -- totalPrice receipts = roundToTwoDecimals $ fold (+) 0 (itNetPrice Items Receipts receipts)
    -- totalPrice thisReceipt = sum $ map (\x-> itNetPrice x) crrList
    --   where crrList = scannedProducts thisReceipt
    --exportReciept

   --Mock
    emptyReceipt= (Receipt [] 0.00)

    appleFP = (Item "Apple" 0.50 NA)
    watermelonFP = (Item "Watermelon" 3.00 NA)
    coffeeFP = (Item "Coffee" 6.00 NA)
    lovageFP = (Item "Lovage" 2.59 NA)
    cheeseFP =  (Item "Cheese" 3.00 NA)
    creamCheeseFP = (Item "CreamCheese" 2.50 NA)
    -- Alice Input
    aliceReceipt = [appleFP,appleFP,appleFP,appleFP,appleFP,watermelonFP,coffeeFP,lovageFP,cheeseFP,cheeseFP,creamCheeseFP]
