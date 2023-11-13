#!/usr/bin/env bash

npm install
./node_modules/.bin/rollup editor.mjs -f iife -p @rollup/plugin-node-resolve -o editor.bundle.js
