---
description: >-
  Here I've copied Daniel Koning's post from the Pre-Alpha Zen forum
  (https://github.com/orgs/Mashweb/teams/pre-alpha-zen). I've also copied hints
  and instructions on optimizations. -Tom Elam
---

# Gambit Scheme

## BiwaScheme to Gambit: the rationale

After comparing the many options listed on the wiki and elsewhere, my conclusion is that Gambit is by far the most promising alternative Scheme to start with. \(To give more background on why it's necessary to find an alternative to Biwa, I'll be posting an addendum soon.\)

After comparing the many options listed on the wiki and elsewhere, my conclusion is that Gambit is by far the most promising alternative Scheme to start with. \(To give more background on why it's necessary to find an alternative to Biwa, I'll be posting an addendum soon.\)

I think it's wise to avoid WebAssembly for now; aside from being opaque and abstruse, it's walled off from the DOM, requiring an interaction layer if it's going to be used as the basis of a page construction tool. That means plain JS has to be the target for the time being.

No currently available Scheme-&gt;JS compiler produces very fast code \(and of course no JS Scheme interpreter is fast either\). However, among the bunch, Gambit seems to do all right. \(In that old paper, there's a benchmark comparison, for what that's worth, and there are much results I'm struggling to turn up again.\) What makes Gambit appealing here is that it already specializes in compiling to C, so if we needed to use WebAssembly for isolated parts, we would have to do very little rewriting, in the ideal case no rewriting at all.

### Installing

The latest numbered release is years old; the JavaScript backend looks to have progressed a lot since then. Clone the [repo](https://github.com/gambit/gambit) and prepare for compilation with:

```text
CC=[/your/path/to/gcc] ./configure --enable-single-host --enable-targets=js
```

GCC rather than Clang is recommended, as Gambit takes advantage of its compiler extensions for speed. Since our build targets will be JS, though, it only affects build time, not runtime efficiency.

Then, where `[N]` is the number of cores on your machine:

```text
make -j[N] && make -j[N] modules && make -j[N] check
sudo make install
```

### Building and trying the demo

```text
cd contrib/try
```

Append this to `extra.scm`:

```text
(define (js-append-paragraph str)
  (##inline-host-statement
   "(function () {
         var p = document.createElement('p');
         p.innerText = @1@;
         document.body.append(p);
    })();"
   str))
```

Then

```text
make try
cd try
```

Serve the directory statically and load it in your browser. \(Don't do it with `make serve`; it tries to do something with SSL and its certificate is no good.\)

A REPL will appear. You should be able to type

```text
(js-append-paragraph "Hello!")
```

and see the new DOM element show up at the bottom of the screen.

Here you have the fundamentals necessary for the Zen client-side stack: a Scheme environment that can manipulate the DOM, and a REPL for talking to the Scheme environment.

Note that Gambit doesn't allow you to pass text in at runtime to be evalled in the backend language. As seen above, you have to hardcode blocks of text at compile time and embed arguments into them as literals.

We could get around this by carefully exposing the JS `eval` through `##inline-host-statement`, if we're willing to leave our Content Security Policy so loose. We shouldn't, though. Better would be to work on an actual API. \(All the same, temporarily exposing `eval` could be useful for quick development.\)

> > [![@tomelam](https://avatars.githubusercontent.com/u/355891?s=60&v=4)](https://github.com/tomelam)**tomelam on 2 Apr**
> >
> > Daniel, that's great information, but I'm wondering how much of the installation and demo are taken from somewhere in the Gambit documentation. Could you please briefly provide some pointers to link what you've written to the original sources? Also, could you comment on the viability of developing Zen applications using Feeley's ["Gambit in Emacs in the browser"](https://feeley.github.io/gambit-in-emacs-in-the-browser/)?

> [![@danielkoning](https://avatars.githubusercontent.com/u/49599680?s=60&u=ef452801fdfb6778d6481510b72990627373f87c&v=4)](https://github.com/danielkoning)**danielkoning on 2 Apr** 
>
> None of this is taken directly from the Gambit docs, except for certain ./configure options and the sequence of targets that you pass to 'make' while building the Gambit core \(1. none, 2. 'modules', 3. 'check'\). Those you can read more about in the base directory's INSTALL.txt.
>
> The rest comes from my reading of Gambit's code and makefiles, along with some knowledge about autotools I've picked up over time. And while the compiler to JavaScript has been in development for a while, 'contrib/try/' is so new that it doesn't even have docs to speak of. So I can't really cite anything except to suggest looking through the code that this post guides you in using.
>
> As for the browser "Emacs," it's impressive \(if light on features\), but it doesn't tell us much about the viability of using Gambit for such applications. Rather, the in-browser editor is a third-party vanilla JS app which provides an interface to a textual Gambit REPL. Gambit isn't handling anything here that has to do with the web app per se.

> [![@tomelam](https://avatars.githubusercontent.com/u/355891?s=60&v=4)](https://github.com/tomelam)**tomelam on 3 Apr**
>
> Thanks. That's quite useful.

### Important note

On the Mac, `gcc` is `clang` by default. `Homebrew` is a good way to install GNU's `gcc`. Daniel's above post describes how to use it. It can also be added to the command path so that `CC=[/your/path/to/gcc]` would be unnecessary.

More information about the installation can be found in [the discussion](https://github.com/orgs/Mashweb/teams/pre-alpha-zen/discussions/7) that Daniel started.

## Optimizing Gambit on JavaScript for Size

I had a [conversation](https://gitter.im/gambit/gambit?at=60aec5ced0f55b33ba0e588e) about reducing the size of Gambit on JavaScript so that a web page can load it faster, mainly with Marc Feeley, the creator of Gambit, and Marc Crampsie. Here are the main points of that conversation.

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

\[I think this might not work for me because I want the initial load of every web page on my web server to include at least a partial Scheme REPL, meaning a subset of Scheme enough to do some reading, evaluation, and printing of Scheme code.\]

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

