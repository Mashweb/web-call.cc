const path = require('path')

const gulp = require('gulp')
const stylus = require('gulp-stylus')
const postcss = require('gulp-postcss')
const autoprefixer = require('autoprefixer')
const uglify = require('gulp-uglify')
const codekit = require('gulp-codekit')
const rollup = require('rollup-stream')
const glob = require('glob')
const babel = require('@rollup/plugin-babel')
const source = require('vinyl-source-stream')
const sourcemaps = require('gulp-sourcemaps')
const rimraf = require('rimraf')
const webpackStream = require('webpack-stream')
const { parallel } = require('gulp')
const stream = require('stream')
const mergeStream = require('merge-stream')

const BUILD_DIRECTORY = path.join(__dirname, 'build')
const SOURCE_DIRECTORY = path.join(__dirname, 'source')


function clean (cb) {
    rimraf.sync(BUILD_DIRECTORY)
    cb()
}

function stylusTask () {    
    const { source, build } = getSourceAndBuildDirectories('stylesheets/stylus')

    return gulp
        .src(path.join(source, '*.styl'))
        .pipe(stylus())
        .pipe(gulp.dest(build))
}

function postCssTask () {
    const { source, build } = getSourceAndBuildDirectories('stylesheets')
    
    return gulp.src(path.join(build, '*.css'))
        .pipe(postcss([autoprefixer()]))
        .pipe(gulp.dest(build))
}


async function images () {
    const imagemin = await import('gulp-imagemin')
    
    return gulp.src(path.join(SOURCE_DIRECTORY, '**/*.png'))
        .pipe(imagemin.default())
        .pipe(gulp.dest(path.join(BUILD_DIRECTORY)))
}


function copy () {
    return gulp.src(path.join(SOURCE_DIRECTORY, '**/*'))
        .pipe(gulp.dest(path.join(BUILD_DIRECTORY)))
}


function getSourceAndBuildDirectories (relativeDirectory) {
    return {
        source: path.join(SOURCE_DIRECTORY, relativeDirectory),
        build: path.join(BUILD_DIRECTORY, relativeDirectory)
    }
}

function bundleJS () {
    const stream = mergeStream()

    glob('source/**/*.js', function (error, files) {
        if (error) {
            throw error;
        }
        
        const tasks = []
        

        for (const input of files) {
            const inputParsedPath = path.parse(input)
            const finalPath = path.join(BUILD_DIRECTORY, 'javascripts', inputParsedPath.name + '.js')
            const finalParsedPath = path.parse(finalPath)
            
            const fs = require('fs')

            if (input.indexOf('biwa') !== -1) {
                continue
            }

            try {
                if (fs.existsSync(finalPath)) {
                    fs.unlinkSync(finalPath)
                }
            } catch (error) {
            }

            const wbp = gulp.src(input)
                .pipe(codekit())
                .pipe(webpackStream({
                    target: 'web',
                    mode: 'production',
                    output: {
                        filename: finalParsedPath.name + finalParsedPath.ext,
                        path: '/'
                    }
                }))
                .pipe(gulp.dest(finalParsedPath.dir))

            stream.add(wbp)
        }
    })

    return stream
}


const styles = gulp.parallel(
    stylusTask,
    postCssTask
)

exports.images = images
exports.copy = copy
exports.bundleJS = bundleJS

exports.bundle = gulp.series(
    clean,
    copy,
    parallel(
        bundleJS,
        styles,
        images,
    )
)
