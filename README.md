# How to run it

- Create an *errorfile* (a file with an sentence containing a mistake in every line). 
  Errors are marked with the following format [error|correction]. Eg:

```
Roug[ez|issez]-vous quelquefois?
```

- Then run the programming with **cabal run** passing as an argument 
  the error file.

```bash
cabal run <errorfile>
```
