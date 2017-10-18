# ALGT2

## What is supported

- Generating parsetrees/parsers based on BNF
- Applying small helper functions (aka reduction rules)
- Building a typechecker or evaluator, based on natural reduction
- Running your functions/typechecker in an interactive environment

See an example implementation of a [simply typed lamda calculaus](ALGT2/blob/master/src/Assets/TestLanguages/STFL.language)

## Getting started

Get started by

- Downloading [ALGT](https://github.com/pietervdvn/ALGT2/raw/master/ALGT) (the executable, linux only) or alternatively `./build.sh`. (There is some preprocessing needed for extra source files)
- Running `ALGT --template` to create a barebones framework
- Rename `Template.language` to `SomethingOfYourChoice.language`
- Running `ALGT . SomethingOfYourChoice`

If you want to experiment with STFL (simply typed lambda calculus), run `ALGT src/Assets/TestLanguages STFL`

## The interactive testing environment

Type any expression, function or relation to execute it, e.g.:

	not("True")
	"True" "&" True → x


Type `help` for an overview of commands.


