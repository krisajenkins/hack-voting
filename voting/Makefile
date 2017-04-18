PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

all: dist/app.min.js \
	dist/main.css \
	.static

dist/main.css: $(shell find styles -type f -name '*.less')
	lessc styles/main.less $@

install:
	npm install
	bower install

dist/app.js: $(shell find src -type f -name '*.purs' -o -name '*.js')
	pulp browserify --to $@

dist/app.min.js: dist/app.js
	uglifyjs $^ -o $@

.static: $(shell find static/ -type f)
	rsync -r static/ dist/

wtest:
	pulp --before clear --watch test

wbuild:
	pulp --before clear --watch build

wbrowse:
	pulp --before clear --watch browserify --to dist/app.js 
