module Bob (bobReceipt)  where
import Items

  pork = (Item "Pork" 4.00 4.00 NA)
  chicken = (Item "Chicken" 4.00 4.00 NA)
  cheese = (Item "Cheese"3.00 3.00 BOGOF)
  mCheese = (Item "Mozzarella Cheese" 0.99 0.99 NA)
  apple = (Item "Apple" 0.50 0.50  BOGOF)
  orange =  (Item "Jaffa Oranges" 3.00 3.00 NA)

   -- Bob Input
  bobReceipt  = [pork,chicken,cheese,mCheese,apple,apple,orange,cheese]
