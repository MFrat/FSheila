<p align="center">
<img src="./img/logo.png">
</p>

# FSheila

[![Build Status](https://travis-ci.org/MFrat/FSheila.svg?branch=master)](https://travis-ci.org/MFrat/FSheila)

>FSheila is a compiler in F# for [IMP Programming Language](https://github.com/ChristianoBraga/BPLC/blob/master/examples/imp/README.md).

## Overview

This compiler is under development as a class assignment; therefore, it is **highly recommended** not to use it for any other proposal.

Class: [TCC-00.289](http://www.ic.uff.br/index.php/en-GB/) ministred by Professor Dr. [Chistiano Braga](http://www2.ic.uff.br/~cbraga/pmwiki/pmwiki.php/Main/AffiliationAndResearchInterests).

Authors: [Erick Grilo](https://github.com/simasgrilo/), [Max Fratane](https://github.com/MFrat/) and [Vítor Lourenço](https://github.com/vitornl/).

## Start Guide

### Prerequisites

* [F# Programming Language](http://fsharp.org/)
* [ScanRat](https://github.com/pragmatrix/ScanRat) parsing expression grammar for F#
* [.Net Core 2.x](https://www.microsoft.com/net/download/linux)
* Faith

##### P.S.: For further information about systems prerequsits check [.Net Core Guide](https://docs.microsoft.com/en-us/dotnet/core/)

### Install, Clean, Build and Run

#### Install all prerequisites

* Linux Debian-Based Distros ```make install```
* MacOS ```make install-macos```

#### Clean, build and run

```
make clean
make build
make run
```
or just
```
make run
```