

data Color = Black | White

instance Eq Color where
  Black == Black  = True
  White == White  = True
  _     == _      = False


class Size a where
  empty :: a
  size :: a -> Int
  sameSize :: a -> a -> Bool

instance Size (Maybe a) where
  empty = Nothing

  size Nothing = 0
  size (Just a) = 1

  sameSize x y = size x == size y

instance Size [a] where
  empty = []
  size xs = length xs
  sameSize x y = size x == size y