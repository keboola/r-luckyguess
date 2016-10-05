#!/bin/bash
echo "Starting tests" >&1

cd /code/
R CMD build .
R CMD check keboola.r.luckyguess_*
grep -q -R "WARNING" "${RCHECK_DIR}/00check.log"
