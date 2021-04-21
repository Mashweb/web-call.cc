---
description: >-
  Most significant web applications use JavaScript features and frameworks to
  handle user interaction state. It is important to understand how they do it so
  that Zen's sequential model can be defended.
---

# Notes on Asynchronous JavaScript

Here we will keep links and key points related to asynchronous JavaScript.

## A Schemer's viewpoint on asynchronous JavaScript to handle user interaction state

The paper ["Whalesong: Running Racket in the Browser"](https://cs.brown.edu/~sk/Publications/Papers/Published/yk-whalesong-racket-browser/paper.pdf) appeared in the Journal of the ACM in 2013. Its abstract read as follows:

### Abstract

JavaScript is the language of the ubiquitous Web, but it only poorly supports event-driven functional programs due to its single-threaded, asynchronous nature and lack of rich control flow operators. We present Whalesong, a compiler from Racket that generates JavaScript code that masks these problems. We discuss the implementation strategy using delimited continuations, an interface to the DOM, and an FFI for adapting JavaScript libraries to add new platform-dependent reactive features. In the process, we also describe extensions to Racket’s functional event-driven programming model. We also briefly discuss the implementation details.

## RxJS

> What is RxJS? RxJS is the reactive extensions library for JavaScript. It's a library for reactive programming using Observables, to make it easier to compose asynchronous or callback-based code. RxJS 6 is a rewrite of Reactive-Extensions/RxJS with better performance, better modularity, better debuggable call stacks, while staying mostly backwards compatible, with some breaking changes that reduce the API surface. Version 7 is currently in public beta, but is currently used by Google in production. To learn more about RxJS, please come to the conference or visit the documentation at rxjs.dev.

### Top sources

1. [RxJS](https://rxjs-dev.firebaseapp.com/)
2. [RxJS Introduction/Overview](https://rxjs-dev.firebaseapp.com/guide/overview)
3. ["RxJS: Reactive Extensions For JavaScript" on npmjs.com](https://www.npmjs.com/package/rxjs)
4. [rxjs.live](https://www.rxjs.live/)
5. [GitHub repo for version 6.x stable](https://github.com/ReactiveX/rxjs/tree/6.x)
6. [RxJS documentation project](https://github.com/ReactiveX/rxjs/tree/6.x/docs_app)

### Miscellaneous but good articles

1. ["An Animated Intro to RxJS" on css-tricks.com](https://css-tricks.com/animated-intro-rxjs/)
2. [GitBook "Learn RxJS"](https://www.learnrxjs.io/)

## ReactiveX

> ReactiveX is a combination of ideas from the observer and the iterator patterns and from functional programming.

### Top sources

1. RxJS is just the reactive extensions library for JavaScript. See ["About RxJS"](https://www.rxjs.live/).
2. ["An API for asynchronous programming with observable streams"](http://reactivex.io/)
3. ["The Interactive Extensions for JavaScript \(IxJS\)"](https://github.com/ReactiveX/IxJS)

## Article series on MDN

1. [Module "Asynchronous JavaScript"](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous)
2. ["General asynchronous programming concepts" \(skippable\)](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Concepts)
3. ["Introducing asynchronous JavaScript" \(callbacks and promises\)](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Introducing)
4. ["Graceful asynchronous programming with Promises"](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Promises)
5. ["Making asynchronous programming easier with async and await"](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await) \(syntactic sugar on top of promises\)
6. ["Choosing the right approach"](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Choosing_the_right_approach)

