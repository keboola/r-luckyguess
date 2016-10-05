#!/bin/bash
echo "Starting tests" >&1

cd /code/
R CMD BUILD .
R CMD CHECK keboola.r.luckyguess_*
grep -q -R "WARNING" "${RCHECK_DIR}/00check.log"
