module Receipt (Receipt(..))
where
    import Items
    import Data.List
    import Numeric
    -- import Data.Decimal
    -- showFDouble (Just 2) theDouble  ""

    data Receipt = Receipt{
        purchasedItems :: [Purchase],
        -- offerApplied :: [Offer],
        -- reducedItems :: [Reduced],
        netPrice :: Double,
        offerSaving ::Double,
        reduceSaving ::Double,
        totalPrice :: Double
    }deriving (Show)

    data Purchase = Purchase{
      itemIndex :: Int,
      itemName :: String,
      itemPrice ::Double
    }deriving (Show)

    data Offer =Offer {
      fstOfferItemIndex :: Int,
      sndOfferItemIndex :: Int,
      offerItemFullPrice :: Double,
      offerPrice ::Double
    }deriving (Show)

    data Reduced =Reduced {
      reducedItemIndex :: Int,
      reducedItemFullPrice :: Double,
      reducedItemPrice ::Double
    }deriving (Show)


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
    mkIndexList :: [Item] -> [(Int,Item)]
    mkIndexList inputList= zip [1..] inputList

    mkPurchase :: (Int,Item) -> Purchase
    mkPurchase inputTuple = Purchase{
    itemIndex = fst inputTuple,
    itemName = itName inputItem,
    itemPrice = itNetPrice inputItem }where inputItem = snd inputTuple

    mkPurchaseList :: [(Int,Item)] -> [Purchase]
    mkPurchaseList [] = []
    mkPurchaseList (x:xs)= (++) (mkPurchase(x): []) (mkPurchaseList(xs))

    getItOfferFromTuple :: (Int,Item) -> OfferType
    getItOfferFromTuple inputTuple = itOffer (snd inputTuple)

    getItNameFromTuple :: (Int,Item) -> String
    getItNameFromTuple inputTuple = itName (snd inputTuple)

    getItemsOnOffer :: [(Int,Item)] ->[(Int,Item)]
    getItemsOnOffer inputTuple = filter (\n -> ( getItOfferFromTuple(n)  == BOGOF)) inputTuple

    getItemsOnReduce :: [(Int,Item)] ->[(Int,Item)]
    getItemsOnReduce inputTuple = filter (\n -> ( getItOfferFromTuple(n)  == REC)) inputTuple

    -- group them
    -- get the name in order
    groupingOfferItems :: [(Int,Item)] -> [(Int,Item)]
    groupingOfferItems inputList = sortBy (\ x y -> compare ( getItNameFromTuple(x)) (getItNameFromTuple(y))) inputList

    getOrderedGroupNames :: [(Int,Item)] -> [[String]]
    getOrderedGroupNames inputList = groupBy (==)( map (\x -> itName x) (map (\x -> (snd x)) (groupingOfferItems inputList)))
    -- getOrderedGroupNamesFromConfig :: [(String,Double)] -> [String]
    -- getOrderedGroupNamesFromConfig inputList = map (\x -> fst x) (sortBy (\ x y -> compare (fst x) (fst y)) inputList)

    --get 1st Indexes

    -- totalPrice
    -- totalPrice :: Receipts -> Double
    -- totalPrice receipts = roundToTwoDecimals $ fold (+) 0 (itNetPrice Items Receipts receipts)
    -- totalPrice thisReceipt = sum $ map (\x-> itNetPrice x) crrList
    --   where crrList = scannedProducts thisReceipt
    --exportReciept

   --Mock
    emptyReceipt= (Receipt [] 0.00)

    bogofItems = ("Apple",0.50) : ("Cheese",3.00) : []

    appleFP = (Item "Apple" 0.50 0.50  BOGOF)
    watermelonFP = (Item "Watermelon" 3.00 3.00 NA)
    coffeeFP = (Item "Coffee" 6.00 6.00 NA)
    lovageFP = (Item "Lovage" 2.59 2.59 NA)
    cheeseFP =  (Item "Cheese" 3.00 3.00 BOGOF)
    creamCheeseFP = (Item "CreamCheese" 2.50 1.00 REC)
    -- Alice Input
    aliceReceipt = [appleFP,appleFP,appleFP,appleFP,watermelonFP,coffeeFP,lovageFP,cheeseFP,cheeseFP,creamCheeseFP,appleFP]
