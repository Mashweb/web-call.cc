---
description: There are seven parts to this book. Here they are described.
---

# Introduction

1. Part 1 of this [_Guide_](https://tomelam.gitbook.io/mashweb/) explains how and why its author began to experiment with ideas related to the simple, easy creation of web pages and web applications.
2. Part 2 lays out the goals and possible goals for Zen and Mashweb.
3. Part 3 documents the nano framework at the heart of Zen.
4. Part 4 documents the plan, of unproven viability, for the construction of web page structures.
5. Part 5 documents the user interaction code that Zen provides to simplify the programming of web apps. Mainly these are protocols for interacting with the HTML5 APIs such as [the HTML Drag and Drop API](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API) and [the Clipboard API](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard_API).
6. Part 6 presents some example web apps such as what a semiskilled web developer might create with [Alpha Zen](https://app.gitbook.com/@tomelam/s/mashweb/part-2-goals-of-zen/the-plan-for-alpha-zen).
7. Part 7 presents outside tools to facilitate the design and programming of Zen and [Mashweb.Club](https://mashweb.club).

Web pages, of course, comprise HTML, CSS, JavaScript, and sometimes other code. The term _web application_ does not have an exact, universally agreed upon definition, but "highly dynamic website" might be a useful way of thinking of a web application as distinct from a static website that has only unreactive web pages. Old-style web applications used just CGI capabilities and had a page-centric progression. When the site visitor submitted input to a web page, the browser jumped to a different web page. Nowadays, however, the loading of a new web page for most interactions is viewed as tedious. Ajax allows a web page to interact with the user and the web without loading a complete new page. A _single page web application_ built using Ajax has the quintessential qualities of what we now view as a web application or _web app_.

When the author was employed by Wipro Technologies, he had the freedom to explore many technologies and to try to build new, practical programming tools and utilities. Realizing that many types of data can most easily be represented and manipulated by Lisp \(or Scheme\), and realizing that S-expressions would be the simplest way to represent web page structure, and learning some of the basics of JavaScript events, he hit upon the idea of using a direct manipulation interface to create and edit web pages.

Later the author discovered a version of Scheme called [jsScheme](https://bluishcoder.co.nz/2006/05/05/scheme-implementation-in-javascript.html) that runs in, or on top of, JavaScript. As some programmers familiar with the Scheme programming language's `call/cc` form realized, the state of progression through a series of interactions can quickly grow overwhelming, but `call/cc` makes it possible to simplify the stateless web and allow it to be programmed in a stateful way.

The author had previously used the UCW \(UnCommon Web\) framework that used Common Lisp to create something like `call/cc` on the web server. His experience with UCW made him realize that a practical, generally useful web framework would have limited scalability due to the huge amount of memory consumed by the `call/cc`-like continuations created for concurrent users of a UCW web application. He realized that by spreading the continuation-related memory consumption across all the web browsers using a web app, that is, by storing the continuations in the web app's pages rather than on the web server, the web app could be scaled much better.

A parallel motivation for building Zen is the author's frustrations and dissatisfaction with web browsers, websites, and utilities for study in the world's new library, the web. These frustrations and dissatisfactions are described in the [author's blog site](https://tomelam.blogspot.com/).

See also the [Zen Elevator Pitches](https://app.gitbook.com/@tomelam/s/mashweb/part-1-the-motivation-behind-zen-and-mashweb/zen-elevator-pitches).

## Part 2: Goals and Possible Goals of Zen

Motivations and goals are not the same thing. When will Zen be complete? Or when will a _version_ of it really exist? The answer is: when its goals have been met.

In reality, though, Zen has a graded set of _possible_ goals, some imminently achievable and some just remotely believable. Part 2 will break this down. See The Plan for Alpha Zen.

## Part 3: The Key to Zen: A Nano Framework for Sequential Programming

Virtually every part of Zen involves intricately designed user interaction. This can be managed only if the asynchronous, stateless environment of JavaScript in the web page can be tamed and brought under the control of a sequential process. It is painful to say it, but it sometimes appears that most of today's web developers believe the most natural way to program user interactions is with pure event driven code. Even seasoned programmers appear to lose sight of the fact that an application that doesn't progress logically through a series of goals is just a random process. Turning complex user interactions into sequential processes by using `call/cc` is Zen's key to success.

## Part 4: Web Page Structure

## Part 5: User Interactions and the HTML5 APIs

## Part 6: Example Web Apps Built with Zen

## Part 7: Outside Tools To Help Build Zen

