module Items (
Item(CartItem), ItemNetPrice, ItemPrice, Offer
) where
    data Item = CartItem{
        ItemName :: String
        ItemNetPrice :: ItemPrice
    }
    data Offer = "Buy2Get1Free" | "Reduced"
    type ItemPrice = Decimal
    
