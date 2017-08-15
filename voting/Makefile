PATH  := node_modules/.bin:$(PATH)
SHELL := /bin/bash

all: webpack

webpack: FORCE
	webpack

prod: FORCE
	webpack --optimize-minimize

dev: FORCE
	webpack-dev-server

test: FORCE
	pulp --before clear test

FORCE:
