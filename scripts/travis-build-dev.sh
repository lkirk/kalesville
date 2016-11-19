#!/bin/sh

body='{
    "request":{
        "message": "Triggering the dev docker build via the api",
        "config": {
	    "script": "make build-web-dev"
        }
    }
}'

curl -s -X POST \
  -H "Content-Type: application/json" \
  -H "Accept: application/json" \
  -H "Travis-API-Version: 3" \
  -H "Authorization: token $TRAVIS_CI_TOKEN" \
  -d "$body" \
  'https://api.travis-ci.org/repo/lloydkirk%2Fkalesville/requests'
