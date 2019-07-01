# A bare-bones differentiation library for teaching






-- 7*x + (x + 9) * x^(2^x):
f = (N 7) :*: (X 1) :+: ((X 1) :+: (N 9)) :*: (X 1) ^. (N 2) ^. (X 1)

-- x*x + x + 1
g = (X 1) :*: (X 1) :+: (X 1) :+: (N 1)

example4 = putStr $ drawTree $ toTree $ f

example5 = drawExp $ (f <> g)
example6 = drawExp $ (g <> f)
example7 = drawExp $ d (g <> f <> g <> f)

example8 = drawExp $ d (f <> g)




