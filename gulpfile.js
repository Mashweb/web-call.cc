const path = require('path')
const fancylog = require('fancy-log')


const gulp = require('gulp')
const stylus = require('gulp-stylus')
const postcss = require('gulp-postcss')
const autoprefixer = require('autoprefixer')
const parcel = require('gulp-parcel')


const rimraf = require('rimraf')

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
    
    return gulp.src(path.join(source, '*.css'))
        .pipe(postcss([autoprefixer()]))
        .pipe(gulp.dest(build))
}


async function images () {
    const imagemin = await import('gulp-imagemin')
    const { source, build } = getSourceAndBuildDirectories('images')

    return gulp.src(path.join(source, '*.png'))
        .pipe(imagemin.default())
        .pipe(gulp.dest(path.join(build)))
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


async function parcelTask () {
    return gulp.src(path.join(SOURCE_DIRECTORY, '**/*.html'), { read: false })
        .pipe(parcel({
            outDir: BUILD_DIRECTORY,
            publicURL: './',
        }))
        .pipe(gulp.dest(BUILD_DIRECTORY))
}

const styles = gulp.parallel(
    stylusTask,
    postCssTask
)

exports.images = images
exports.copy = copy
exports.parcelTask = parcelTask
exports.styles = styles

exports.bundle = gulp.series(
    clean,
    copy,
    styles,
    images,
    parcelTask,
)