# ocaml-bitap
Bitap fuzzy search for OCaml

A very simple port of the bitap fuzzy search algorithm from the DiffMatchPatch library https://code.google.com/p/google-diff-match-patch/
No dependency.

Usage:
```
  match_bitap text pattern start
```

Example:
```
  match_bitap "Lorem ipsum dolor sit amet, consectetur adipiscing elit" "consectatur" 0
  -> 28
```
