SWMMoutGetMB
============

A parser for SWMM 5 binary .OUT files written in Haskell

Introduction
------------

The United States Environmental Protection Agency (EPA) Storm Water Management
Model (SWMM) is a dynamic hydrology-hydraulic water quality simulation model for single event or
long-term (continuous) simulation of runoff quantity and quality from primarily urban areas.

SWMM 5, which is currently the newest version of SWMM, produces a binary `.OUT` file as its
output. SWMMoutGetMB is a SWMM binary reader using the Get monad in Haskell to retrieve 
contents of this binary file and save it into a `SWMMObject`, which can then be used in 
Haskell programs.

Installation
------------

You would first need the haskell-platform. The installation of this depends on your OS, but if
you're on Ubuntu, you can use the following terminal commands:

~~~ bash
$ sudo apt-get install haskell-platform
~~~

Then, you need to get the libraries required for parsing binary data:

~~~ bash
$ cabal update
$ cabal install binary
$ cabal install data-binary-ieee754
$ cabal install bytestring
$ cabal install split
$ cabal install datetime
~~~

Usage
-----

The parser can take in a lazy bytestring, and return a `SWMMObject` which contains all the
information present in the `.OUT` file. The `example.hs` file currently present just prints out this
`SWMMObject`, but it should be possible to do any sort of manipulations with the `SWMMObject`,
such as writing it out to a CSV file.

To see the output produced by printing the `SWMMObject`, simply run the program:

~~~ bash
$ runhaskell example.hs
~~~

To demonstrate how flexible the parser can be, the program
[swmmout2csv](https://github.com/OOW/swmmout2csv) is implemented using the
parser. Example code is present in `swmmout2csv.hs`.

To run `swmmout2csv.hs`, simply use:

~~~ bash
$ runhaskell swmmout2csv.hs
~~~

SWMM OUT FILE FORMAT
--------------------

The SWMM `.OUT` file has a certain format it follows, and this format is documented in the
`format` file. In case this format changes, the updated format should be available on the [SWMM
website](http://www2.epa.gov/water-research/storm-water-management-model-swmm) under the downloads
section (See SWMM Interfacing Guide). These files may however be windows help files, with a
`.chm` format. The important details of that file are mentioned in the `format` file in this repo.

