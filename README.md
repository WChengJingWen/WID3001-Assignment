# WID3001 — Assignment: Functional Logic & Programming

## Arithmetic Expression Evaluator — Debug, Refactor & Extend

---

## Repository Structure

```
src/
├─ AST.hs                -- abstract syntax tree definitions
├─ Tokenizer.hs          -- lexical analysis
├─ Parser.hs             -- expression parser
├─ EvaluatorCore.hs      -- pure evaluator
├─ Main.hs               -- CLI entry point
├─ Tests.hs              -- HUnit test suite
Evaluator_Original.hs     -- original flawed version (reference)
Evaluator_FixedBug.hs     -- intermediate debugging version
README.md                 -- this file
```

---

## How to Run the Program?
```bash
cd src
ghci
:load Main.hs
main "2+5"
```
## Test Suite

The `Tests.hs` file includes **28 HUnit tests**.

Run via:

```bash
cd src
ghci Tests.hs
main
```


