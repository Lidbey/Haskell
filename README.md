# Haskell
Some haskell code(very short)

It basically takes as an input things like:
(I (C (I (S 'A') (S 'B')) (I (S 'B') (S 'C'))) (I (S 'A') (S 'C')))
and outputs it as this:
I-If (=>)
C-Conjunction (&)
A-Alternative (|)
N-Not (~)
S-Element
It also does logic on it, finds if it is tautology
