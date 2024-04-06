# Strelitzia

Suppose you have installed a [minimal TeX Live](https://github.com/indrjo/minimal-texlive-installer.git) and you want to produce a pdf via, say,

``` sh
$ pdflatex main.tex
```

It probably won't work owing to missing packages from a still younger TeX Live. It is a work for *strelitzia*:

``` sh
$ strelitzia main.tex
```

It coordinates a couple of actors: it runs `pdflatex main.tex`, extracts from the log the names of the missing files, invokes *tlmgr* to determine the parent packages and makes *tlmgr* install them. How the command terminates (zero o non-zero exit code) is not relevant here.

**Warning.** The dependencies of TeX Live are a hell: that means you may have to run *strelitzia* more than once to get all dependencies fully satisfied.

Once your ecosystem is enough for your work, you can go back to using `pdflatex main.tex`, as you are used.


## Usage and options

```sh
$ strelitzia [OPTIONS] MAIN.tex
```

* The default compiler is `pdflatex`, but you can change this with `--engine ENGINE`. Valid alternatives are for example `lualatex`, `xelatex` and so on...

* You may want *strelitzia* to check whether all the packages required by the current project are installed before attempting the creation of the final document. Use `--check-imports PREAMBLE.tex` to indicate the file where are the *import* statements. So far, the program is able to detect the following keywords for mustering packages: `\usepackage`, `\documentclass` and `\RequirePackage`.

* The selected engine will halt at every error and wait for manual intervention by the user. To ignore and continue, it is sufficient to hit `Enter`. By default, *strelitzia* is able to do this for you for up to `50` times. If this number is exceeded, the user has to press the key `Enter` as many times it will be needed. If you feel this limit is too low (it may be so in case of large projects and a very *young* TeX Live), you can increase this value with `--max-halts NUM`. 

**Warning.** In general, always refer to

```sh
$ strelitzia --help
```

because it may happen the latest changes are not properly documented yet.


## Installation

This program is written in [Haskell](https://www.haskell.org/), so you need all the machinery to compile the source code: if you do not have it, [click!](https://www.haskell.org/ghcup/) There is a `Makefile` here, so use it:

```sh
# Install strelitzia in ~/.local/bin.
$ make
# Create documentation in ~/.local/bin.
$ make doc
# Uninstall strelitzia.
$ make uninstall
```

**Important.** Make sure `~/.local/bin` is in your `PATH`.


## Troubleshooting

...

