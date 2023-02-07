SOURCES = $(wildcard *.org)
HTML = $(SOURCES:.org=.html)
PDF = $(SOURCES:.org=.pdf)
CHROME = $(shell which google-chrome-beta || which google-chrome-stable || which google-chrome)
MAYBE_CHROME_PATH = $(if $(CHROME),--chrome-path $(CHROME),)

ONEPDF=all-slides.pdf

all: $(HTML)
.PHONY: all

%.html: %.org flake.nix
	nix run .# -- "$<"

pdf: $(PDF)
.PHONY: pdf

# Use a specific window size to work around this issue:
# https://github.com/astefanutti/decktape/issues/151
%.pdf: %.html
	nix run --no-sandbox .#decktape -- -s 2048x1536 $(MAYBE_CHROME_PATH) "$<" "$@"

one-pdf: $(ONEPDF)
.PHONY: one-pdf

$(ONEPDF): $(PDF)
	nix run .#pdfunite -- $(PDF) $(ZIPPREFIX).pdf

clean:
	rm -f $(HTML)
	rm -f $(PDF)
	rm -f $(ONEPDF)
.PHONY: clean
