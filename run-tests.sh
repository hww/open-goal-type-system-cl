#!/bin/bash
rlwrap sbcl --non-interactive --load type-system.asd --eval '(ql:quickload :type-system)' --load run-tests.lisp
