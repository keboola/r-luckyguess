#!/bin/bash
echo "Starting tests." >&1

export KBC_DATADIR=/code/tests/data/
cd /code/
R CMD build .

echo "Build finished." >&1
R CMD check keboola.r.luckyguess_*
if [ $? -eq 0 ] ; then
	echo "Test passed successfully." >&1
	grep -q -R "/code/keboola.r.luckyguess.Rcheck/00check.log"
	if [ $? -eq 0 ] ; then
		echo "No warnings found." >&1
	else
		echo "Warnings found." >&2
		cat /code/keboola.r.luckyguess.Rcheck/00check.log
		exit 1
	fi
else
	echo "Tests failed." >&2
	cat /code/keboola.r.luckyguess.Rcheck/00check.log
	cat /code/keboola.r.luckyguess.Rcheck/tests/testthat.Rout.fail
	exit 1
fi
