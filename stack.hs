type Stack a = [a]

inPila :: Stack a -> a -> Stack a
inPila q x = [x] ++ q

fuoriPila:: Stack a -> Stack a
fuoriPila [] = error "Pila Vuota"
fuoriPila (x:xs) = xs

empty :: Stack a -> Bool
empty [] = True
empty (x:xs) = False

front :: Stack a -> a
front [] = error "Pila Vuota"
front (x:xs) = x