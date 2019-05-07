# example_grammars

## Introduction

This repository contains examples to supplement the main parsing libraries I work on.

## Quick links

* P0 parser at <https://github.com/tomjridge/p0/>
* OCamldoc at <http://tomjridge.github.io/example_grammars/>

## P0, ABNF example

At the moment, the main example is a sequence of P0 parsers. The first implements a parser for "Plain BNF". Subsequent parsers culminate in a parser for  the IMAP protocol. See <./src/README.md> for further details. 

A fragment of the generated ABNF parser is:

![1557223111833](README.assets/1557223111833.png)

A fragment of the IMAP grammar (from the RFC) used to generate the IMAP parser, is:

![1557223376198](README.assets/1557223376198.png)