API is the main access point for outside programmers. It contains all you need to work with language definitions

Utils: In this folder you'll find some of the pieces that the modules here need, such as...
	- ExceptionInfo (and Failable): the monad handling failure
	- Checkable: a typeclass representing stuff that can be validated
	- Grouper: a fancy implementation of an ordered dictionary
	- LocationInfo: data structures keeping track of what is notated where

Data: the definitions and parsers of all data structures needed for a languagedef
