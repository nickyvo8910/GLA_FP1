module Items (
Item(..), Offer
) where
    import Data.Decimal 

    data Item = Item{
        itName :: String,
        itNetPrice :: Decimal
    } deriving (Show, Eq, Ord)
    data Offer = BOGOF | Reduced | NA 
    


    
