module Items (
Items(..)
, Offer(..)
) where
    import Data.Decimal 

    data Items = Items{
        itName :: String,
        itNetPrice :: Decimal,
        itOffer :: Offer
    } deriving (Show, Eq, Ord)
    

    data Offer = BOGOF | Reduced | NA deriving (Show, Eq, Ord)
    


    
