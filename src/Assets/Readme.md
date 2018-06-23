
 What are assets?
==================

Assets are a hole bunch of files which are needed in all kinds of places throughout the program.

Luckily, these files don't have to change, so we link them in statically. The static linking is done by a haskell module 'Assets.hs'.
Assets.hs basically defines a lot of strings, such as `_ALGT_TestLanguages_STFL_Language`. This string will contain the contents of the file _at compile time_.

Assets.hs is created automatically by `Utils.CreateAssets.autoCreateDevAssets`. Assets.hs can be constructed in two ways: with all those strings hardcoded or with a call to `unsafePerformIO . readFile`. The first one is used when actually building, but is very slow to load in the interpreter (not to mention a text editor). For interpreting, we have the second variation. 


 Overview of the assests
=========================

 There are a few classes of assets:

 - The syntax of .language-files is written down in other, simpler .language file.
		ALGT is more or less bootstrapped. The syntax and some helper functions are defined in `MetaSyntax`
 - ALGT contains more advanced parts of the syntax. Especially ALGT/Native/* is the same as in MetaSyntax, either through automatically copying them or via a symlink
 - TestInput contains a plethora of (faulty) languages used to test error checks
 - Resources contains mainly documentation (e.g. strings explaining how to use the interpreter and clarifying diagrams)
