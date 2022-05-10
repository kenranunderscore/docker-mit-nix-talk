SOURCES = $(wildcard *.org)
HTML = $(SOURCES:.org=.html)

all: $(HTML)

%.html: %.org
	emacs --batch -q -l export.el --eval='(org-re-reveal-export-file  "$<" "$(REVEAL_ROOT)" "$(REVEAL_MATHJAX_URL)")'

clean:
	rm -f $(HTML)
.PHONY: clean
