.PHONY: doc clean uninstall

.RECIPEPREFIX = >

# where the binary and the documentations go
INSTALLDIR = $(HOME)/.local/bin

PROG_PATH = $(INSTALLDIR)/strelitzia
DOC_PATH = $(INSTALLDIR)/strelitzia-doc.pdf

$(PROG_PATH): src/*.hs *.cabal
> @cabal build -O2 -p
> @cabal install --overwrite-policy=always --installdir=$(INSTALLDIR)

doc: $(DOC_PATH)

$(DOC_PATH): README.md
> @pandoc -V 'colorlinks' -o $(DOC_PATH) $<

clean:
> @cabal clean

uninstall:
> @rm -fv $(PROG_PATH)
> @rm -fv $(DOC_PATH)

