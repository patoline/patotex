# PatoTeX

PatoTeX is an interface to Patoline which inputs files written in a
subset of LaTeX syntax. It aims to parse the largest part of LaTeX
syntax. Yet, it only aims at parsing high-level LaTeX commands,
excluding any low-level hacks since we do not intend to reimplement TeX
macro expansion rules.

The goal is to provide Patoline users with an easy upgrade path,
allowing them to render existing documents using Patoline without
rewriting all source files.

## Getting started

PatoTeX depends uses:
* [ocaml-earley](https://github.com/rlepigre/ocaml-earley) for parsing input files,
* [ocaml-earley-ocaml](https://github.com/rlepigre/ocaml-earley-ocaml)
  is needed to build PatoTeX iself,
* and [Patoline](https://patoline.github.io/) for typesetting content.

Clone the PatoTeX repository:
```
git clone https://github.com/patoline/patotex.git
```
and build PatoTeX by simply running
```
make
```
in the source directory.

This produces a `patotex` executable which you can use as follows:
```
patotex some_file.tex
```
If everything goes well, this produces the `some_file.pdf` file.

## License

PatoTeX is released under the GNU General Public License version 3. See
the `LICENSE` file for the full licensing terms.

## Contributing

Contributions are obviously welcome. Do not hesitate to report issues or
send pull requests.

In OCaml source files, please avoid using tabulations. Use spaces for
indentation.
