HUGOCMD=hugo
HUGOPTS=--quiet
HUGO_PUBLISHDIR=$(HOME)/co/com.gkayaalp/dev/elisp
export HUGO_PUBLISHDIR
ORGSRCDIR=../..
export ORGSRCDIR
HEAD!=git rev-parse HEAD

help:
	@echo "Targets:"
	@echo "	build:		build the website"
	@echo "	generate:	generate pages from org source"
	@echo "	serve:		start the build server"

# FIXME: Do not remove index.xml here, make hugo not create it.
build: generate
	rm -rf $(HUGO_PUBLISHDIR)/* &&\
	$(HUGOCMD) $(HUGOPTS) &&\
	rm $(HUGO_PUBLISHDIR)/index.xml

.PHONY: build

# FIXME: Any way to not re-export when uptodate?
generate:
	emacs --script generate.el

.PHONY: generate

serve:
	cd $(HUGO_PUBLISHDIR)/.. && python -m SimpleHTTPServer

.PHONY: serve
publish: build
	cd $(HUGO_PUBLISHDIR) && git add . \
		&& git commit -m "elisp@$(HEAD)" && git push
