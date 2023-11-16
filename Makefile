SHELL = /bin/bash

build-analysis-binary:
	rm -f analysis/rescript-editor-analysis.exe
	dune build
	cp ./_build/default/analysis/bin/main.exe analysis/rescript-editor-analysis.exe

build-tests:
	make -C analysis/tests build

build: build-analysis-binary build-tests

dce: build-analysis-binary
	opam exec reanalyze.exe -- -dce-cmt _build -suppress vendor

clean:
	rm -f analysis/rescript-editor-analysis.exe
	dune clean
	make -C analysis/tests clean
	make -C analysis/reanalyze clean

test-analysis-binary: build-analysis-binary
	make -C analysis/tests test

test-reanalyze: build-analysis-binary
	make -C analysis/reanalyze test

test: test-analysis-binary test-reanalyze

format:
	dune build @fmt --auto-promote

checkformat:
	dune build @fmt

.DEFAULT_GOAL := build

.PHONY: build clean test
