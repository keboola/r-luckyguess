#!/bin/bash

docker login -u="$QUAY_USERNAME" -p="$QUAY_PASSWORD" quay.io
docker tag keboola/r-luckyguess quay.io/keboola/r-luckyguess:$TRAVIS_TAG
docker tag keboola/r-luckyguess quay.io/keboola/r-luckyguess:latest
docker images
docker push quay.io/keboola/r-luckyguess:$TRAVIS_TAG
docker push quay.io/keboola/r-luckyguess:latest
