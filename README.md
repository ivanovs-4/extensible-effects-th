Module `Control.Eff.Operational.TH` exports single method `mkProgramOps`

Let's say your DSL is represented by data structure
```haskell
-- | Define data using GADTs.
data DSL a where
   ReadString :: DSL String
   WriteString :: String -> DSL ()
```
Note `mkProgramOps` only works with GADTs for now.

Then
```haskell
mkProgramOps ''DSL
```
will generate functions
```haskell
writeString :: Member (Program Box) r => String -> Eff r ()
writeString = singleton . WriteString

readString :: Member (Program Box) r => Eff r String
readString = singleton ReadString
```

Full example is [Simple.hs](./examples/Simple.hs)
You can build and execute it:
```
stack build --flag extensible-effects-th:examples
stack exec ee-simple
```
