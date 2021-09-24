const { Transform } = require('stream')
const path = require('path')

const gulp = require('gulp')
const stylus = require('gulp-stylus')
const postcss = require('gulp-postcss')
const autoprefixer = require('autoprefixer')
const include = require('gulp-codekit')
const glob = require('glob')
const rimraf = require('rimraf')
const webpackStream = require('webpack-stream')
const mergeStream = require('merge-stream')
const GulpUglify = require('gulp-uglify')


const BUILD_DIRECTORY = path.join(__dirname, 'build')
const SOURCE_DIRECTORY = path.join(__dirname, 'source')

function clean (cb) {
    rimraf.sync(BUILD_DIRECTORY)
    cb()
}

function getGulpIncludeStream () {
    return include({ 
        includePaths: [
            path.join(__dirname, 'node_modules'),
            path.join(SOURCE_DIRECTORY, 'javascripts')
        ],
        extensions: 'js',
        hardFail: true,
        separateInputs: true,
    })

}


const webpackEnabledFiles = [
    'finder-vaadin-polymer.js',
]

function stylusTask () {    
    return gulp.src(path.join(SOURCE_DIRECTORY, '**/*.styl'), { base: '.' })
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


function bundleNonWebpackJS () {
    return gulp.src(path.join(SOURCE_DIRECTORY, `**/biwa*widgets*control*.js`), { base: '.', dirMode: false })
    .pipe(getGulpIncludeStream())
    .pipe(new Transform({
        readableObjectMode: true,
        writableObjectMode: true,
        transform: (chunk, enc, callback) => {
            chunk.contents = chunk.contents.toString()
            callback(null, {})

        }
    }))
    .pipe(GulpUglify({
        compress: {
            dead_code: false
        },
    }))
    .pipe(gulp.dest(BUILD_DIRECTORY, { overwrite: true }))
}

function bundleWebpackJS () {
    const stream = mergeStream()

    glob(path.join(SOURCE_DIRECTORY, `**/+(${webpackEnabledFiles.join('|')}|*.module.js)`), function (error, files) {
        if (error) {
            throw error;
        }
        
        for (const input of files) {
            const inputParsedPath = path.parse(input)
            const inputFileName = inputParsedPath.name + inputParsedPath.ext
            const finalPath = path.join(BUILD_DIRECTORY, 'javascripts', inputFileName)
            const finalParsedPath = path.parse(finalPath)

            const fs = require('fs')

            try {
                if (fs.existsSync(finalPath)) {
                    fs.unlinkSync(finalPath)
                }
            } catch (error) {
            }

            const wbp = gulp.src(input)
                .pipe(getGulpIncludeStream())
                .pipe(new Transform({
                    readableObjectMode: true,
                    writableObjectMode: false, 
                    transform: (chunk, enc, callback) => callback(null, chunk.contents)
                }))
                .pipe(webpackStream({
                    target: 'web',
                    mode: 'production',

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
                          },
                        ],
                      },
                      
                      optimization: {
                          sideEffects: false,
                          minimize: false
                      }
                }))
                .pipe(gulp.dest(finalParsedPath.dir))

            stream.add(wbp)
        }
    })

    return stream
}

const bundleJS = gulp.series(
    bundleNonWebpackJS,
    // bundleWebpackJS,
)

const styles = gulp.parallel(
    stylusTask,
    postCssTask
)

exports.bundleJS = bundleJS
exports.images = images
exports.copy = copy
exports.bundle = gulp.series(
    clean,
    copy,
    gulp.parallel(
        bundleJS,
        styles,
        images,
    )
)
