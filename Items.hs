module Items (
Item(..)
, OfferType(..)
) where
    -- import Data.Decimal

    data Item = Item{
        itName :: String,
        itNetPrice :: Float,
        itOffer :: OfferType
    } deriving (Show, Eq, Ord)


    data OfferType = BOGOF | Reduced | NA deriving (Show, Eq, Ord)
