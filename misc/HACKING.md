# Development guide

## Build

To build a development version, install cabal and ghc and run:

```shell
$ git clone https://github.com/owo-lang/OwO
$ cd OwO
$ cabal install alex
$ cabal install
```

You can also use stack, no problem:

```shell
$ git clone https://github.com/owo-lang/OwO
$ cd OwO
$ stack install alex
$ stack install
```

To install bash completions temporarily, run:

```shell
$ source <(owo --bash-completion-script $(which owo))
```

To install bash completions for all users permanently, run:

```shell
$ owo --bash-completion-script $(which owo) >> owo_completion
$ sudo mv owo_completion /etc/.bash_completion.d/
```

You can run `hlint` with this command:

```shell
$ hlint src
```

## Testing

The tests are split into two parts -- Haskell tests and golden-valued tests.
This is because stack is too powerful on Windows.

### Golden-valued tests

You need perl5 \> v5.10 and a bash environment to run golden-valued tests.
For Windows, you can use `git-bash`. It works like a charm.

See [this instruction](../src/test/testData/README.md).

## Coding convention

* Functions/record projections should be camelCase

* The prefix `Psi` stands for *Program Structure Item*, it's for concrete syntax elements
  * contextual syntax errors can be detected here
* The prefix `Ast` stands for *Abstract Syntax Tree*, it's for abstract syntax tree
  * no syntactic sugars
  * no unresolved references

## Setup your editor

### IntelliJ-Haskell

You're basically all set.

### Code

Install the `stylish-haskell` extension to format the code easier.
You should always format your code before pushing it.

It's recommended to use drammie's `Simple GHC (Haskell) Integration` plugin.
Configure your workspace:

```json
{
    "ghcSimple.workspaceType": "stack"
}
```

You can also install the extension `Run On Save` to simply run the formatter on
saving your `*.hs` file, by giving such commands in settings:

```json
{
    "emeraldwalk.runonsave": {
        "commands": [
            {
                "match": "\\.hs$",
                "cmd": "stylish-haskell -i ${file}"
            }
        ]
    }
}
```

### Emacs

You should have `stylish-haskell` installed:

```shell
$ cabal install stylish-haskell
```

Then, add this to your `.emacs` file:

```elisp
(custom-set-variables
 '(haskell-stylish-on-save t))
```

So that you can format your code when you save them.
