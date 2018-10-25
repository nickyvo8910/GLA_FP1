module Charlie (charlieItems)
where
  import Items

  pork = (Item "Pork" 4.00 2.00 REC)
  apple = (Item "Apple" 0.50 0.50  BOGOF)
  bananas = (Item "Bananas" 0.80 0.80 NA)
  cheese = (Item "Cheese" 3.00 1.00 REC)


   -- Charlie Input
  charlieItems  = [pork,apple,bananas,cheese,cheese,cheese]
