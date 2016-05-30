type Queue a = [a]

enq :: Queue a -> a -> Queue a
enq q x = q ++ [x]

deq :: Queue a -> Queue a
deq [] = error "Coda Vuota"
deq (x:xs) = xs

empty :: Queue a -> Bool
empty [] = True
empty (x:xs) = False

front :: Queue a -> a
front [] = error "Coda Vuota"
front (x:xs) = x
