<!--- OASIS_START --->
<!--- DO NOT EDIT (digest: 6707b52271aed22f3c105961d9f0f667) --->

This is the INSTALL file for the foo distribution.

This package uses OASIS to generate its build system. See section OASIS for
full information.

Dependencies
============

In order to compile this package, you will need:

* ocaml
* findlib
* ppx_tools for executable ppx_patch
* compiler-libs for executable ppx_patch

Installing
==========

1. Uncompress the source archive and go to the root of the package
2. Run 'ocaml setup.ml -configure'
3. Run 'ocaml setup.ml -build'
4. Run 'ocaml setup.ml -install'

Uninstalling
============

1. Go to the root of the package
2. Run 'ocaml setup.ml -uninstall'

OASIS
=====

OASIS is a program that generates a setup.ml file using a simple '_oasis'
configuration file. The generated setup only depends on the standard OCaml
installation: no additional library is required.

<!--- OASIS_STOP --->