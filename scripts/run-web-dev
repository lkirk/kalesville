#!/bin/bash
trap 'kill %1; kill %2' SIGINT
APP_ENV=docker\
       /home/user/.roswell/bin/clackup \
       --server :hunchentoot \
       --port 5000 \
       app.lisp &
npm run dev
