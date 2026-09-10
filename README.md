# haskell-incentives-dsl

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/). The recommended way to install stack is by using [ghcup](https://www.haskell.org/ghcup/).
It's also possible to use [the nix package manager](https://nixos.org/), but this project does not provide a nix configuration file.

Assuming `stack` is installed in the system, to **build** the project use
```
stack build
```
To **build and run the tests**, run
```
stack test
```
or equivalently
```
stack build --test
```
For **faster feedback loop** during development, it's possible to run tests continuously on every file change:
```
stack test --fast --file-watch
```
To run tests with **test coverage** instrumentation,
```
stack test --coverage
```
which generates a textual and HTML report.

Note: Tests are run in the CI and test coverage reports are automatically uploaded to codecov.

To **run** the executable via slack,
```
stack exec haskell-incentives-dsl
```
or passing arguments
```
stack exec haskell-incentives-dsl -- -v doctor
```

To **install** the executable under `~/.local/bin`,
```
stack install
```
and the executable can be run with `haskell-incentives-dsl` assuming `~/.local/bin` is in the `$PATH` variable.

To run a version of **ghci** compatible with the resolver
```
stack ghci
```
For more information about how to use `stack`, refer to the [official docs](https://docs.haskellstack.org/en/stable/).

This projects uses [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) to implement command-line arguments parsing. Optparse-applicative automatically generates an helper from code. It's recommended to use the generated helper to explore all the available CLI options.