module Items (
Item(..)
, OfferType(..)
) where
    -- import Data.Decimal

    data Item = Item{
        itName :: String,
        itNetPrice :: Double,
        itReducedPrice :: Double,
        itOffer :: OfferType
    } deriving (Show, Eq, Ord)


    data OfferType = BOGOF | REC | NA deriving (Show, Eq, Ord)
