##
##   $ make deploy
##   $ cd dist
##   $ python -m SimpleHTTPServer 8000
##

SML_LIB= ~/gits/mlkit/js
JS_PATH_FLAGS=-js_path_compress -js_path_relative_to $(SML_LIB)
SMLTOJS=SML_LIB=$(SML_LIB) ~/gits/mlkit/bin/smltojs

.PHONY: all
all: index.html

page.html: main.mlb
	$(SMLTOJS) $(JS_PATH_FLAGS) -o page $<

index.html: static/page_start.html page.html static/page_end.html
	cat static/page_start.html > $@
	cat page.html >> $@
	cat static/page_end.html >> $@

.PHONY: highchart
highchart: highchart.mlb
	SML_LIB=$(SML_LIB) $(SMLTOJS) -c $<

.PHONY: clean
clean:
	rm -rf MLB *~ index.html page.html prims.js basis dist

DEPLOYDIR=dist

.PHONY: deploy
deploy: index.html
	rm -rf $(DEPLOYDIR)
	mkdir -p $(DEPLOYDIR)
	cp -pa $(SML_LIB)/prims.js $(SML_LIB)/basis $(DEPLOYDIR)
	cp -pa static/favicon.ico index.html MLB $(DEPLOYDIR)
	mkdir -p $(DEPLOYDIR)/isodate
	cp -pa isodate/MLB $(DEPLOYDIR)/isodate
	mkdir -p $(DEPLOYDIR)/listutil
	cp -pa listutil/MLB $(DEPLOYDIR)/listutil
	mkdir -p $(DEPLOYDIR)/lib/github.com/diku-dk/sml-sort
	cp -pa lib/github.com/diku-dk/sml-sort/MLB $(DEPLOYDIR)/lib/github.com/diku-dk/sml-sort
