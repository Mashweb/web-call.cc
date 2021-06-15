---
description: >-
  I had a conversation about reducing the size of Gambit on JavaScript so that a
  web page can load it faster, mainly with Prof. Marc Feeley, the creator of
  Gambit, and Drew Crampsie. Here are the main p
---

# Optimizing Gambit on JavaScript for Size

> Tom Elam @tomelam 03:33 @feeley I haven't studied the building of the online Gambit REPL \([https://github.com/gambit/gambit/tree/master/contrib/try](https://github.com/gambit/gambit/tree/master/contrib/try)\) but I wonder if you could point me to a way to reduce the built size of the JavaScript for that. It was over 11 MB when I built it. Is there a part of it \(say, without some of the SRFI code or some of the math code\) that can be built by redefining some make or shell variable? Something like that? I'd like to have an online Gambit REPL in just 1 or 1.5 MB of JavaScript. ... Redefining a make target would be a good option, too, if it wouldn't make the build process fail.
>
> Tom Elam @tomelam 03:40 @rajeevn1 @feeley I've heard about the Gambit smart linker. Any idea when it will be ready for a test drive?

> Drew Crampsie @drewc 04:14 @tomelam did you configure with `--enable-default-compile-options=(compactness 9)`? Here's what I have for a "repl" \(minus codemirror and browserFS\):

```text
du -hs VM.min.js*
  4.9M    VM.min.js
  688K    VM.min.js.gz   
```

> Drew Crampsie @drewc 04:20 Having said that, I have in the past dropped the .js size down to 1 meg or so, BUT, not the REPL. The REPL is basically a full gambit environment, as is required for read and eval and print, so cannot really be reduced more than that. This is the point where I point out this:

```text
find /nix/ -name 'libgambit.so' -exec du -h {} \;
8.5M /nix/store/m27913wknikn3nkqiny6j0m68kily4np-gambit-unstable-2020-07-29/gambit/lib/libgambit.so
```

> So, in JavaScript it ends up 1/2 the size of C? Not quite the same thing, of course, but really helped to give me perspective.

> Drew Crampsie @drewc 04:25 If you do not "lexicalize" certain toplevels the .gz can drop to almost 640k ... I just had to do that for gerbil packages essentially. and the 4.9 is like 3.2 or somesuch. Can almost fit on a 5-1/4 floppy? Small enough for my purposes :\).

> Marc Feeley @feeley 04:30 @tomelam Please read the comments at the top of contrib/try/makefile.in:

```text
# To build VM.js from VM.scm you need a recent Gambit with the JavaScript
# backend.  It can be compiled like this (from an existing bootstrapped
# recent installation):
#
#   cd gambit
#   git pull
#   make clean
#   ./configure --enable-single-host --enable-targets=js --enable-default-compile-options="(compactness 9)" --prefix=/usr/local/Gambit
#   make
#   make doc
#   make modules
#   sudo make install   (optional)
```

> The `VM.js` file \(the bulk of the Gambit online REPL\) will be postprocessed by the google closure compiler and when the result is gzipped you will get a 640KB file. If you want to go even smaller, you can trim some stuff from the runtime library, such as support for complex numbers, exact rationals and bignums by using `configure` options such as `--disable-cpxnum`, etc. To enable the “smart linker” you need to do a “whole-program” compilation that includes the file `lib/_univlib.scm` and prefix the code with `(declare (optimize-dead-definitions))`. Note that this will remove some of the runtime library so your REPL won’t be able to access the definitions that were removed \(it is a compilation strategy that works best for complete applications\).

> Bradley Lucier @gambiteer 06:49 I don't think anyone has thoroughly tested the runtime with --disable-cpxnum, etc. Just a comment.

> Tom Elam @tomelam May 27 11:38 @feeley @drewc Thank you both immensely! I am very excited about this!

> Tom Elam @tomelam May 27 19:35 @gambiteer Thanks for the heads up!

> Marc Feeley @feeley May 27 19:40 @tomelam Don’t hesitate to ask more questions here if you get stuck.

> Marc Feeley @feeley May 28 18:02 @tomelam … For your use it might be simplest to just take the VM.min.js.gz file “as is” and to write the rest of your app as a module that is loaded by the runtime system using `load` or `import` or `eval`. What programming language features are needed by your app?

> Marc Feeley @feeley May 28 19:57 @tomelam To get you started, here is how to compile a minimal web app to Scheme… First you should build Gambit with:

```text
./configure --enable-single-host --enable-targets=js --enable-default-compile-options="(compactness 9)”
make
make modules
```

> This will build the JS version of the Gambit runtime library and produce compact JS code. Then create a `mini-web-app` directory and put in it this `makefile`, `index.html` and `app.scm`:

```text
all: app.min.js

app.js: app.scm
    gsc -target js -label-namespace "z" -exe -o app.js app.scm

app.min.js: app.js
    npx google-closure-compiler --language_in=ECMASCRIPT_2015 --language_out=ECMASCRIPT_2015 --js app.js --js_output_file app.min.js
    sed -I .tmp -e "s/^'use strict';//" app.min.js
    gzip -k -9 app.min.js  # optional but useful if the web server can send .gz files

clean:
    rm -f app.js app.min.js app.min.js.gz
```

```text
<!doctype html>
<html>
  <head><script src="app.min.js"></script></head>
  <body onload="scheme_program_start();"></body>
</html>
```

```text
;;; File: "app.scm"

(include "~~lib/_gambit#.scm")
(include "~~lib/_six/js#.scm")

(##inline-host-declaration #<<end-of-host-code

// Defer Scheme code execution until scheme_program_start is called.
scheme_program_start = @all_modules_registered@;
@all_modules_registered@ = function () { };

end-of-host-code
)

(define alert \alert)
(define prompt \prompt)
(define (body-html-set! html) \document.body.innerHTML=`html)

(body-html-set! (string-append "<h1>"
                               (prompt "Please enter your name")
                               "</h1>"))

(thread-sleep! 0.1) ;; let browser update the screen

(alert "The body now contains your name!\nClick OK to replace it with the New-York weather forecast.\n")

(define (fetch-json url)
  \fetch(`url).then(function (r) { return r.json(); }))

(define x
  (fetch-json "https://forecast.weather.gov/MapClick.php?lat=40.78333&lon=-73.96667&FcstType=json"))

(define temp \(`x).currentobservation.Temp)
(define name \(`x).currentobservation.name)

(body-html-set! (string-append "<h1>" name ": " temp "F</h1>"))

;; uncomment the following to start a REPL:
;;(thread-sleep! 0.1) ;; let browser update the screen
;;(##repl-debug-main)
```

> Then after a `make` just open `index.html` in a browser.

> Tom Elam @tomelam May 28 20:13 @feeley The programming language features I need are rich flow control forms, including also call/cc, a arithmetic operations \(at least block floating point\), hopefully a module system, the ability to load a full Scheme implementation bit by bit, and a foreign function interface for JavaScript.

> Marc Feeley @feeley May 28 20:14 @tomelam You’re all set with Gambit then!

...

> Marc Feeley @feeley May 28 23:05 @tomelam I did a quick check and I think Gambit is the right fit for your needs because it directly implements your “sequential programming” model in the browser… I’m not sure I understand your need for call/cc \(it is probably better to use Scheme threads to have independent computations\), but of course call/cc is there if it is required… concerning event handling, please have a look at this paper, especially section 5.2: [http://www.iro.umontreal.ca/~feeley/papers/BelangerFeeleyELS21.pdf](http://www.iro.umontreal.ca/~feeley/papers/BelangerFeeleyELS21.pdf)

...

> Drew Crampsie @drewc May 29 21:09 @tomelam ... I do have to say that "full Scheme in the browser" and "quick initial load of a REPL in the browser" are somewhat in opposition to each other, depending on what you mean by "full" and "quick".

> Drew Crampsie @drewc May 29 23:58 ... and it very much depends on what you mean by "quick" when it comes to loading. I personally find that Gambit is more than fast enough and for an application can be put in a lot of local storage type places with cache and the like. Even without that my applications, even in dev mode, load faster than gmail and/or facebook. In other words, Gambit is more than enough and the hard work they put in for both compactness and the `\syntax(that).I.thought({ I: "would"}).dis("like")` but in fact like quite a bit makes it superior over every other lisp/scheme I've used in the browser over the years.

