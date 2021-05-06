#!/usr/bin/env bash

emacs -Q -batch -l ert -l mmark-test.el -f ert-run-tests-batch-and-exit
