# Gensl/gsl: Generic Spell Language

## Roadmap

Version 0.1 Checklist

- [x] basic parsing algorithm
- [x] basic features
  - [x] string, symbol, bytes, boolean, numeric atoms
  - [x] positional and keyword nodes
  - [x] annotation nodes and annotated datum
  - [x] mixfix syntax
  - [x] codified symbol atoms [#1](https://github.com/kxcteam/gensl/issues/1)
  - [x] complex form styles: list, vector, set, map [#2](https://github.com/kxcteam/gensl/issues/2)
        (partially done: missing multi-dimensional vector support)
- [ ] complete 4 representations (Parsetree, Datatree, Normaltree, Canonicaltree)
  - [ ] pretty-printers for each representations [#3](https://github.com/kxcteam/gensl/issues/3)
  - [ ] forgetting and embedding functions between representations [#4](https://github.com/kxcteam/gensl/issues/4)
  - [ ] builder for each representation [#5](https://github.com/kxcteam/gensl/issues/5)
- [ ] migration to sedlex (no unicode support) [#6](https://github.com/kxcteam/gensl/issues/6)
- [ ] perfect unparse (Parsetree --> Wirestring) [#7](https://github.com/kxcteam/gensl/issues/7)
- [ ] ordering and equality [#8](https://github.com/kxcteam/gensl/issues/8)
  - [ ] canonical ordering
  - [ ] structural equality + semantical equivalence
- [ ] proper testing
  - [ ] proper unit testings
  - [ ] (forget . embed) among representations constitute identity
  - [ ] forgetting functions accepts all inputs
  - [ ] (x : Datatree) => Parsetree => Wirestriing could be parsed back to x
  - [ ] (parse . unparse) and (unparse . parse) constitute identity, except latter may see parsing error

Roadmap to Version 1.0-ish

- [ ] better string support
  - [ ] `(str|..|str)` strings
- [ ] phantom element support
  - [ ] comma, mapsto support
- [ ] representational tree traversal library
  - [ ] datum access via path
  - [ ] zipper library
- [ ] unicode support
  - [ ] unicode symbols
  - [ ] unicode string
  - [ ] proper codepoint range sanity check + normalization handling (NFKC for symbols and NFC for strings)
- [ ] binary encoding
- [ ] canonical hashing
- [ ] hole and partial incorrect input parsing support
- [ ] custom lexer support
- [ ] preprocessor support
  - [ ] preprocessor atom
  - [ ] preprocessor lexing element
  - [ ] preprocessor special forms

Roadmap to Version 2.0-ish

- [ ] pattern support
  - [ ] pattern language design
  - [ ] pattern match support (for schema check and info extraction)
  - [ ] transform via pattern support

Nice to have features

- [ ] conversions to and from JSON
- [ ] conversions to and from language values
  - [ ] ocaml record/list/variant
  - [ ] javascript object/array
