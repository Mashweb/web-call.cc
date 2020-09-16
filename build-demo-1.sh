#!/usr/bin/env bash

if [[ -z "$(node --version)" ]] ; then
  echo "Node.js not found, please install it first!"
fi

echo "Running npm init ..."
npm init --yes

echo "Installing @medv/finder ..."
npm i @medv/finder

echo "Installing @vaadin/vaadin ..."
npm i @vaadin/vaadin --save

echo "Installing Rollup ..."
npm i rollup @rollup/plugin-node-resolve >/dev/null 2>&1

echo "Installing Typescript plugin for Rollup ..."
npm install @rollup/plugin-typescript --save-dev

echo "Running rollup using rollup.config.js ..."
npx rollup --config >/dev/null 2>&1

echo "Done. Open index.html in your browser."
echo "Run 'npx rollup --config' or 'npx rollup -c' to regenerate the bundle when necessary."
