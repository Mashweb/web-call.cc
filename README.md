# web-call.cc

Source code and design documents for the web-call.cc website.

## Building

Edit index.js if you like and run the following in this directory
in a suitable shell such as Bash:
```
npm init --yes >/dev/null
npm i @medv/finder >/dev/null 2>&1
npm i rollup @rollup/plugin-node-resolve >/dev/null 2>&1
npx rollup --config >/dev/null 2>&1```

## Rebuilding

Edit index.js, then run `npx rollup --config` to regenerate `bundle.js`.
