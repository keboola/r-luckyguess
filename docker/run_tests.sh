#!/bin/bash
echo "Starting tests" >&1

export KBC_DATADIR=/data/
cd /code/
R CMD build .
R CMD check keboola.r.luckyguess_* && grep -q -R "/code/keboola.r.luckyguess.Rcheck/00check.log"
