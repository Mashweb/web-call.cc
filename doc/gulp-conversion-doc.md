# Gulp Conversion Document

This document is about gulpfile.js and guidelines on how to make further 
changes.

## About CodeKit Configuration

CodeKit configuration is being replaced with Gulpfile.

All the current CodeKit files are still within the repo but with new names. All the CodeKit related files will have .codekit extension at the end.


## Gulp Tasks for different formats

Below, you'll find description of how different file types are processed and the corresponding tasks for them


### What is a gulp task?

Gulp Task is essentially a function either wrapped in `gulp.task` or exported directly from gulpfile.js,  all tasks can be individually run using gulp cli too using the following file format

```sh
gulp [filename1] [filename2]
```

### JavaScript 

JavaScript has two tasks associated with it 

- bundleWebpackJS
- bundleNonWebpackJS

#### bundleWebpackJS

This task is going to be used for bundling javascript code that makes use of 
ES6 imports. 

#### bundleNonWebpackJS

This task is going to be used for bundling rest of the javascript code. The rationale behind two bundling tasks is that, many of the non webpack bundles are coded to exist in the global scoe as they have interdependency with each other. Hence they can't be bundled with Webpack, as it wraps them in IIFE. 


Both of these also support `gulp-include`

### CSS 

CSS has two tasks associated with it 

- postCSSTask
- stylusTask

Both of these tasks also rely on `gulp-clean-css` for css minification.

### Image Process

Image processing relies on `gulp-imagemin` and the corresponsing gulp task in
gulpfile is `gulp-imagemin`


### Gulp Include

The include comments are processed through gulp-include and respective
changes have been made for this processing to occur. If you want to remove this 
please look for tasks that have `gulp-include` within their chain


## Adding a new gulp package

Adding a new gulp package requires first finding a package that seems fit for the job and then either creating a new task or if one of the glob already processes for the files you can also add this task to that chain if applicable.

