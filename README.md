# PaLleRo

## Goals

* Detect ambiguities at parser generation time and error out.
* Delayed decisions e.g. statements like `a[0] = a[0];` or `(a, b) = (1, 2);`
  should work without having to merge lval/pattern rules with expr rules and
  then manually validate/transform the syntax tree when '=' or ';' finally
  comes along. (I think this is the only reason why e.g. Lua is not LL(k)).
* Don't generate lookup tables.

## Non-Goals

* Parse any CFG.
* Be super-algorithmic like PEG.

