# A bare-bones differentiation library for teaching

This is a bare-bones library for performing differentiation of elementary functions, written in Haskell.
It was presented as part of a single-variable calculus course, to show that the process of differentiation
is completely algorithmic and can be carried out at the level of parse trees.

The exercise `ancient-egyptian-algebra.tex` gets at much the same idea. I present this before
students have learned about derivatives/differentials, because many students who "learned" how
to take derivatives in a prior class did not learn the process correctly, and need encouragement
to unlearn their bad habits.  (Note: the exercise actually computes *differentials* rather than
derivatives per se, as does the library.)

```haskell
-- 7*x + (x + 9) * x^(2^x):
f = (N 7) :*: (X 1) :+: ((X 1) :+: (N 9)) :*: (X 1) ^. (N 2) ^. (X 1)

-- x*x + x + 1
g = (X 1) :*: (X 1) :+: (X 1) :+: (N 1)

example4 = putStr $ drawTree $ toTree $ f

example5 = drawExp $ (f <> g)
example6 = drawExp $ (g <> f)
example7 = drawExp $ d (g <> f <> g <> f)

example8 = drawExp $ d (f <> g)
```



