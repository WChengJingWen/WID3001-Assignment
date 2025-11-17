# WID3001 Functional Logic and Programming - Assignment 

## How to execute the program?
1. Check whether cabal is installed.
```bash
cabal --V
```

2. Build the project.
```bash
cabal build
```
3. Run the project.
```bash
cabal run WID3001-Assignment "2+3"
cabal run WID3001-Assignment "(10 - 5) * 2"
cabal run WID3001-Assignment "5 / 0"
cabal run WID3001-Assignment "2 + + 3"
```

4. Run test cases.
```bash
cabal test
```