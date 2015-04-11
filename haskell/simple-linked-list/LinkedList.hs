module LinkedList(nil,isNil,next,toList,fromList,reverseLinkedList,datum,new) where


data LL a = Cons {datum :: a, next :: LL a} | Nil

nil :: LL a
nil = Nil

new :: a -> LL a -> LL a
new = Cons

isNil :: LL a -> Bool
isNil Nil = True
isNil _   = False

toList :: LL a -> [a]
toList Nil        = []
toList (Cons a b) = a:toList b

fromList :: [a] -> LL a
fromList = foldr Cons Nil

reverseLinkedList :: LL a -> LL a
reverseLinkedList xs = rll xs Nil
  where 
    rll Nil        ys = ys
    rll (Cons a l) ys = rll l (Cons a ys)


