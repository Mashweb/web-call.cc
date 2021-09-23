# Gulp Migration Spec

This lists all the gulp alternatives that are going to be used for Web-Call in order to migrate from CodeKit.

Source Folder: source/
Output Folder: build/


## Gulp File Format Specifications

Separate specifications for all the file formats and 
their respective gulp handlers. 

### Catch-All

All the files that don't have a Gulp Module specified would be 
copied as is from `source` to `build` with the same relative file hierarchy.

Following are the catch all file formats

- xml
- yaml
- codekit3
- [NO_EXTENSION]
- txt
- gitattributes
- ttf
- scm


## Alternative for Specially Treated Files

Following are the alternatives for the files that are specially treated

### Stylus

Gulp Module: https://www.npmjs.com/package/gulp-stylus

Code Kit Configuration: In CodeKit Config under the key `languageDefaultsSTYLUS`

Input Directory: `stylesheets/stylus`


### PostCSS 

Gulp Module: https://www.npmjs.com/package/gulp-postcss

Files To Processed using PostCSS: all the .css files in /stylesheets


### ttf 

Gulp Module: Null

Description: Would be copied as is


### jpeg and other image optimizations

Gulp Module: https://www.npmjs.com/package/gulp-imagemin


### eslint

Gulp Module: https://www.npmjs.com/package/gulp-eslint

EslintRules: As specified in config.codekit3

### html files minification

Gulp Module: https://www.npmjs.com/package/gulp-htmlmin

### JavaScript Minification

Gulp Module: https://www.npmjs.com/package/gulp-uglify

Configuration Extracted from following keys under `config.codekit3`

- `uglifyMangleNames`
- `uglifyReservedNamesString`
- `uglifyFlags2`
- `uglifyDefinesString`

### JSON minification

Using gulp-uglify for this purpose as well
Output: [RELATIVE_DIRECTORY]/[filename]-min.json

### Gulp PurgeCSS

Gulp Module: https://www.npmjs.com/package/gulp-purgecss

### Markdown (MultiMarkdown)

Gulp Module: https://github.com/jjlharrison/gulp-multimarkdown

Notes: Since CodeKit uses MultiMarkdown a compatible module is going to be used here. Read the CodeKit page on [markdown](https://codekitapp.com/help/markdown/)

