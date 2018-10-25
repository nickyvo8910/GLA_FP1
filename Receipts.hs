module Receipt (Receipt(..))
where
    import Items
    import Data.List
    import Data.Function
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

 -----------------------------------------------------------------------------------
    --Mock input

    bogofItems = ("Apple",0.50) : ("Cheese",3.00) : []

    appleFP = (Item "Apple" 0.50 0.50  BOGOF)
    watermelonFP = (Item "Watermelon" 3.00 3.00 NA)
    coffeeFP = (Item "Coffee" 6.00 6.00 NA)
    lovageFP = (Item "Lovage" 2.59 2.59 NA)
    cheeseFP =  (Item "Cheese" 3.00 3.00 BOGOF)
    creamCheeseFP = (Item "CreamCheese" 2.50 1.00 REC)
     -- Alice Input
    aliceReceipt = [appleFP,appleFP,appleFP,appleFP,watermelonFP,coffeeFP,lovageFP,cheeseFP,cheeseFP,creamCheeseFP,appleFP]
    crrReceipt =[]++aliceReceipt
 -----------------------------------------------------------------------------------
     --Shared Resource

     --Scanned Products in order
     --return [(1,A),(2,B),..]
    scannedProducts :: [Item] -> [(Int,Item)]
    scannedProducts inputList= zip [1..] inputList

     --Getters for Item from a tuple

    getItOfferFromTuple :: (Int,Item) -> OfferType
    getItOfferFromTuple inputTuple = itOffer (snd inputTuple)

    getItNameFromTuple :: (Int,Item) -> String
    getItNameFromTuple inputTuple = itName (snd inputTuple)

    -- return netPrice if not on Reduced and reducedPrice otherwise
    getItPriceFromTuple :: (Int,Item) -> Double
    getItPriceFromTuple inputTuple
     |(itOffer inputItem) == REC = itReducedPrice inputItem
     |otherwise = itNetPrice inputItem
     where inputItem = snd inputTuple

 -----------------------------------------------------------------------------------
     -- Make a list of Purchases

     -- Make single entry
    mkPurchase :: (Int,Item) -> Purchase
    mkPurchase inputTuple = Purchase{
     itemIndex = fst inputTuple,
     itemName = itName inputItem,
     itemPrice = itNetPrice inputItem }where inputItem = snd inputTuple

     --Put them into List
    mkPurchaseList :: [(Int,Item)] -> [Purchase]
    mkPurchaseList [] = []
    mkPurchaseList (x:xs)= (++) (mkPurchase(x): []) (mkPurchaseList(xs))
 -----------------------------------------------------------------------------------
      -- Make a list of Offers

      --Get the names of items that are on offer (pre-defined from CashMachine.hs)
      --["Apple","Cheese",..]
    getOrderedGroupNamesFromConfig :: [(String,Double)] -> [String]
    getOrderedGroupNamesFromConfig inputList = map (\x -> fst x) (sortBy (\ x y -> compare (fst x) (fst y)) inputList)

    -- Filter out items from scannedProducts that are on offer
    getItemsOnOffer :: [(Int,Item)] ->[(Int,Item)]
    getItemsOnOffer inputTuple = filter (\n -> ( getItOfferFromTuple(n)  == BOGOF)) inputTuple

    -- Filter out items on offer that has a same name
    filterOfferItemWithKeyWord :: [(Int,Item)] -> String -> [(Int,Item)]
    -- groupingOfferItems inputList = sortBy (\ x y -> compare ( getItNameFromTuple(x)) (getItNameFromTuple(y))) inputList
    filterOfferItemWithKeyWord inputList inputName = filter (\n -> ( getItNameFromTuple(n)  == inputName)) inputList

    -- If there is an odd number of items from the same type then scrap the last element
    -- as it won't be on offer
    evenUpOfferList :: [(Int,Item)] -> [(Int,Item)]
    evenUpOfferList [] = []
    evenUpOfferList (x:[]) = []
    evenUpOfferList (x:xs) = if (length (x:xs) `mod` 2 /= 0)
      then (take ((length (x:xs))-1) (x:xs))
      else (x:xs)

    --Spliting up even and odd position --> To pair (1,2)(3,4) with the help of the pairOfferItems function
    splitOfferList :: [(Int,Item)]-> ([(Int,Item)],[(Int,Item)])
    splitOfferList [] = ([], [])
    splitOfferList [x] = ([x], [])
    splitOfferList (x:y:xs) = (x:xp, y:yp) where (xp, yp) = splitOfferList xs

    --Pair them up
    pairOfferItems :: ([(Int,Item)],[(Int,Item)]) ->[((Int,Item),(Int,Item))]
    pairOfferItems inputList= zip (fst inputList ) (snd inputList )
      -----------------------------------

    --Processing the offer for one name
    --Filter the offer with item name
    --evenUpOfferList
    --splitOfferList
    --pairOfferItems
    -- return [(A1,A2),(A3,A4)]
    processOfferItemWithKeyword :: String ->[((Int,Item),(Int,Item))]
    processOfferItemWithKeyword  inputKeyword = pairOfferItems (splitOfferList(evenUpOfferList(filterOfferItemWithKeyWord(getItemsOnOffer(scannedProducts(crrReceipt))) (inputKeyword))))

    --For each item name in offer
    --process them
    --concat them into [[((Int,Item),(Int,Item))]]

    groupingOfferItemWithKeywordList ::  [String] -> [[((Int,Item),(Int,Item))]]
    groupingOfferItemWithKeywordList inputKeywords =  map processOfferItemWithKeyword inputKeywords

    --Normalise the data as we don't need order in here
    preSortOffer :: [[((Int,Item),(Int,Item))]] ->[((Int,Item),(Int,Item))]
    preSortOffer inputList = concat inputList

    -- sort processed Offers by 2nd index of 2nd item
    -- as the order of appliedOffer depends entirely on the order of the later item
    sortOffer :: [((Int,Item),(Int,Item))] -> [((Int,Item),(Int,Item))]
    sortOffer inputList = sortBy (\ x y -> compare (fst(snd x)) (fst(snd x))) inputList

    test = sortOffer(preSortOffer(groupingOfferItemWithKeywordList(getOrderedGroupNamesFromConfig(bogofItems))))


    --mkSingleOffer
    mkSingleOffer :: ((Int,Item),(Int,Item)) -> Offer
    mkSingleOffer inputTuple = Offer{
      fstOfferItemIndex = fst (fst inputTuple),
      sndOfferItemIndex = fst (snd inputTuple),
      offerItemFullPrice = itNetPrice (snd (snd inputTuple)),
      offerPrice = (/) (itNetPrice (snd (snd inputTuple))) 2.00
      }
    --mkAppliedOffers

-----------------------------------------------------------------------------------
     -- Make a list of Reduced Items


    getItemsOnReduce :: [(Int,Item)] ->[(Int,Item)]
    getItemsOnReduce inputTuple = filter (\n -> ( getItOfferFromTuple(n)  == REC)) inputTuple

    -- getReducedItems :: [(Int,Item)] ->












    --mkAppliedOffers
    -- mkAppliedOffers :: [((Int,Item),(Int,Item))] -> [Offer]
    -- mkAppliedOffers [] = []
    -- mkAppliedOffers (x:[]) = []
    -- mkAppliedOffers (x:xs) = [(fst x), (itNetPrice (snd x))]
    -- sortAppliedOffers




    -- -- group them
    -- -- get the name in order
    -- groupingOfferItems :: [(Int,Item)] -> [(Int,Item)]
    -- groupingOfferItems inputList = sortBy (\ x y -> compare ( getItNameFromTuple(x)) (getItNameFromTuple(y))) inputList
    --
    -- getOrderedGroupNames :: [(Int,Item)] -> [[String]]
    -- getOrderedGroupNames inputList = groupBy (==)( map (\x -> itName x) (map (\x -> (snd x)) (groupingOfferItems inputList)))
    --
    -- --get 1st Indexes (index A)
    -- get1stIndexes :: [[String]] ->[Int]
    -- get1stIndexes inputList = [length x | x <- inputList]

    -- totalPrice
    -- totalPrice :: Receipts -> Double
    -- totalPrice receipts = roundToTwoDecimals $ fold (+) 0 (itNetPrice Items Receipts receipts)
    -- totalPrice thisReceipt = sum $ map (\x-> itNetPrice x) crrList
    --   where crrList = scannedProducts thisReceipt
    --exportReciept

    -- groupingOfferItems ::  [(Int,Item)] -> [String] -> [(Int,Item)]
    -- groupingOfferItems inputList inputNames = [(filterOfferItemWithKeyWord inputList x)| x<-inputNames]
