---
description: >-
  Here I've copied Daniel Koning's post from the Pre-Alpha Zen forum
  (https://github.com/orgs/Mashweb/teams/pre-alpha-zen). -Tom Elam
---

# BiwaScheme to Gambit: the rationale

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

