
import resolve from '@rollup/plugin-node-resolve'
// This plugin is needed when you import npm modules. See:
// https://rollupjs.org/guide/en/#warning-treating-module-as-external-dependency

export default {
  input: 'index.js',
  output: {
    file: 'bundle.js',
    format: 'iife', // Wrap our code into a self-executing function, see: https://rollupjs.org/guide/en/#quick-start
  },
  plugins: [resolve()]
}

