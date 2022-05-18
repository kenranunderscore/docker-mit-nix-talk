SOURCES = $(wildcard *.org)
HTML = $(SOURCES:.org=.html)
PDF = $(SOURCES:.org=.pdf)
CHROME = $(shell which google-chrome-beta)

all: $(HTML)
.PHONY: all

%.html: %.org
	nix develop -c emacs --batch -q -l export.el --eval='(org-re-reveal-export-file  "$<" "$(REVEAL_ROOT)" "$(REVEAL_MATHJAX_URL)")'

%.pdf: %.html
	nix run --no-sandbox .#decktape -- -s 1920x1080 --chrome-path $(CHROME) "$<" "$@"

clean:
	rm -f $(HTML)
.PHONY: clean
