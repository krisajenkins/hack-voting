all: dist/app.js dist/index.html dist/main.css dist/interop.js .tested

dist/main.css: $(shell find styles -type f -name '*.less')
	lessc styles/main.less $@

dist/app.js: $(shell find src -type f -name '*.elm' -o -name '*.js') dist
	elm-make src/App.elm --yes --warn --output=$@

dist:
	@mkdir $@

dist/%.html: static/%.html dist
	cp $< $@

dist/%.js: static/%.js dist
	cp $< $@

dist/%.gif: static/%.gif dist
	cp $< $@

dist/%.png: static/%.png dist
	cp $< $@

dist/%.ico: static/%.ico dist
	cp $< $@

.tested: $(shell find src vendor tests -type f -name '*.elm' -o -name '*.js')
	$(MAKE) -C tests
	touch $@

TEMPFILE := $(shell mktemp "$$TMPDIR/$$(uuidgen).js")

data/languages.json: FORCE
	firebase database:get /events/languages/options | python -m json.tool > $@

data/projects.json: FORCE
	firebase database:get /events/projects/options | python -m json.tool > $@

FORCE:
