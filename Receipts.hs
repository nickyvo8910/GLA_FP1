module Receipts (Receipt(..),checkout)
where
    import Items
    import Data.List
    import Data.Decimal
    data Receipt = Receipt{
        onOfferProducts    
        scannedProducts[..],
        netPrice :: Decimal
    }

    }
    -- checkout
    checkout :: [Item] -> Receipt

    -- scanNew
    scanNew :: Receipt -> Item -> Quantity -> Receipt
    -- checkOffer 0 NA -1 BOGOF else Reduced
    checkOffer :: Item -> Decimal
    -- applyOffer
   
    -- totalPrice
    totalPrice :: Receipt -> Decimal
    --exportReciept

    -- Alice Input
    appleFullPrice = Item 0.50 None
    
