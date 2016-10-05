#!/bin/bash
echo "Starting tests." >&1

export KBC_DATADIR=/data/
cd /code/
R CMD build .

echo "Build finished." >&1
if [ R CMD check keboola.r.luckyguess_* -eq 0 ] ; then
	echo "Test passed successfully." >&1
	if [ grep -q -R "/code/keboola.r.luckyguess.Rcheck/00check.log" ] ; then
		echo "No warnings found." >&1
	else
		echo "Warnings found." >&2
		cat /code/keboola.r.luckyguess.Rcheck/00check.log
		exit 1
	fi
else
	echo "Tests failed." >&2
	cat /code/keboola.r.luckyguess.Rcheck/00check.log
	exit 1
fi
