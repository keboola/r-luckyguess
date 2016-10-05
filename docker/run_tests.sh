#!/bin/bash
echo "Starting tests" >&1

export KBC_DATADIR=/data/
cd /code/
R CMD build .
R CMD check keboola.r.luckyguess_* && grep -q -R "/code/keboola.r.luckyguess.Rcheck/00check.log"

if [ $? -eq 0 ]
then
  echo "Test passed successfully."
else
  echo "Tests failed." >&2
  cat /code/keboola.r.luckyguess.Rcheck/00check.log
fi
