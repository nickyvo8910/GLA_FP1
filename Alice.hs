module Alice (aliceReceipt)  where
import Items
  appleFP = (Item "Apple" 0.50 0.50  BOGOF)
  watermelonFP = (Item "Watermelon" 3.00 3.00 NA)
  coffeeFP = (Item "Sumatran Coffee Beans" 6.00 6.00 NA)
  lovageFP = (Item "Lovage" 2.59 2.59 NA)
  cheeseFP =  (Item "Cheese" 3.00 3.00 BOGOF)
  creamCheeseFP = (Item "Cream Cheese w. BlackPepper" 2.50 1.00 REC)
   -- Alice Input
  aliceReceipt = [appleFP,appleFP,appleFP,appleFP,appleFP,watermelonFP,coffeeFP,lovageFP,cheeseFP,cheeseFP,creamCheeseFP]
