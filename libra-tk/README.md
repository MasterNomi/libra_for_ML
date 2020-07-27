# The Libra Toolkit, version 1.1.2c:

## LICENSE

You are welcome to use the code for research or commercial purposes
under the terms of the 2-clause BSD license.  However, we request that
you acknowledge its use with a citation, or at least a reference to
the Libra home page: http://libra.cs.uoregon.edu

If you like, please also drop us a line about what you do with Libra and what
results you obtain.  See the LICENSE file for official license information.

NOTE: Libra includes several external projects that it depends on:
ocaml-expat and lbfgs.  Their licenses may need to be acknowledged as
well.  See the Developer's Guide for more information.

## INSTALLATION

Libra requires that OCaml is installed, including the `ocamlfind` tool,
the `ocaml-expat` library, and `oasis`. The recommended method for installing
these is through OPAM, the OCaml package manager. Installation instructions
are system dependent, but may be found online:
[http://opam.ocaml.org/doc/Install.html](http://opam.ocaml.org/doc/Install.html)

For example, on macOS OPAM might be installed with [homebrew](https://brew.sh/):

```bash
brew install opam
```

Linux distributions will generally use the built-in package manager:

```bash
dnf install opam
```

Once installed, `opam` is likely to install the latest version of OCaml by
default. We will need to use 4.02 for Libra:

```bash
opam switch create 4.02.1
opam init
```

Additional dependencies may be installed using:

```bash
opam install ocamlfind ocaml-expat oasis
```

To build Libra from source on UNIX-like hosts (Linux, MacOS, etc.) run:

```bash
make
```

The default installation prefix is `/usr/local`. To change it, run:

```bash
./configure --prefix=<path>
```

To install Libra, run:

```bash
make install
```

## DOCUMENTATION

Please see the `doc/` directory for official documentation:

- `doc/manual.pdf`:   Overview of Libra's functionality, including a "quick start" guide with a short tutorial.
- `doc/devguide.pdf`: Information on the code architecture of Libra

To generate library documentation:

```bash
make doc
```

and browse `doc/html/index.html`

## MISC

Please contact Daniel Lowd <lowd@cs.uoregon.edu> and Amirmohammad
Rooshenas <pedram@cs.uoregon.edu> with any questions, comments,
bug fixes, etc.
