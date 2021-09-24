const path = require('path')

const gulp = require('gulp')
const stylus = require('gulp-stylus')
const postcss = require('gulp-postcss')
const autoprefixer = require('autoprefixer')
const codekit = require('gulp-codekit')
const glob = require('glob')
const rimraf = require('rimraf')
const webpackStream = require('webpack-stream')
const { parallel } = require('gulp')
const mergeStream = require('merge-stream')

const BUILD_DIRECTORY = path.join(__dirname, 'build')
const SOURCE_DIRECTORY = path.join(__dirname, 'source')


function clean (cb) {
    rimraf.sync(BUILD_DIRECTORY)
    cb()
}

function stylusTask () {    
    return gulp.src(path.join(SOURCE_DIRECTORY, '**/*.styl'))
        .pipe(stylus())
        .pipe(gulp.dest(BUILD_DIRECTORY))
}

function postCssTask () {
    return gulp.src(path.join(SOURCE_DIRECTORY, '**/*.css'), { base: '.' })
        .pipe(postcss([autoprefixer()]))
        .pipe(gulp.dest(BUILD_DIRECTORY))
}


async function images () {
    const imagemin = await import('gulp-imagemin')
    
    return gulp.src(path.join(SOURCE_DIRECTORY, '**/*.png'), {base: '.'})
        .pipe(imagemin.default())
        .pipe(gulp.dest(BUILD_DIRECTORY))
}


function copy () {
    return gulp.src(path.join(SOURCE_DIRECTORY, '**/*'))
        .pipe(gulp.dest(path.join(BUILD_DIRECTORY)))
}

function bundleJS () {
    const stream = mergeStream()

    glob('source/**/*.js', function (error, files) {
        if (error) {
            throw error;
        }

        const ignoredFiles = [
            'biwascheme_terminal.js',
            'biwascheme-0.6.9.js',
        ]

        for (const input of files) {
            const inputParsedPath = path.parse(input)
            const inputFileName = inputParsedPath.name + inputParsedPath.ext
            const finalPath = path.join(BUILD_DIRECTORY, 'javascripts', inputFileName)
            const finalParsedPath = path.parse(finalPath)

            const fs = require('fs')

            if (ignoredFiles.some(filename => inputFileName === filename)) {
                continue
            }

            try {
                if (fs.existsSync(finalPath)) {
                    fs.unlinkSync(finalPath)
                }
            } catch (error) {
            }

            const wbp = gulp.src(input)
                .pipe(webpackStream({
                    target: 'web',
                    mode: 'development',
                    output: {
                        filename: finalParsedPath.name + finalParsedPath.ext,
                        path: '/'
                    },
                    module: {
                        rules: [
                          {
                            test: /\.m?js$/,
                            exclude: /(node_modules|bower_components)/,
                            use: {
                              loader: 'babel-loader',
                              options: {
                                presets: ['@babel/preset-env']
                              }
                            }
                          }
                        ]
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
