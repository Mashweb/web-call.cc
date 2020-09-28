#!/usr/bin/env bash

if [[ -z "$(node --version)" ]] ; then
  echo "Node.js not found, please install it first!"
fi

echo "Installing packages ..."
npm install

echo "Running rollup using rollup.config.js ..."
npx rollup --config >/dev/null 2>&1

echo "Done. Open index.html in your browser."
echo "Run 'npx rollup --config' or 'npx rollup -c' to regenerate the bundle when necessary."
