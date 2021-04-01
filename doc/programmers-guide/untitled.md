---
description: There are three parts to this book. Here they are described.
---

# Introduction

## Part 1: The Motivation Behind Zen and Mashweb

Part 1 of this book explains how and why its author began to experiment with ideas related to the simple, easy creation of web pages and web applications. Web pages, of course, comprise HTML, CSS, JavaScript, and sometimes other code. _Web application_ does not have an exact, universally agreed upon definition, but "highly dynamic website" might be a useful way of thinking of a web application as distinct from a static website that has only unreactive web pages. Old-style web applications used just CGI capabilities and had a page-centric progression. When the site visitor submitted input to a web page, the browser jumped to a different web page. Nowadays, however, the loading of a new web page for most interaction is viewed as tedious. Ajax allows a web page to interact with the user and the web without loading a complete new page. A _single page web application_ built using Ajax has the quintessential qualities of what we now view as a web application or _web app_.

When the author was employed by Wipro Technologies, he had the freedom to explore many technologies and to try to build new, practical programming tools and utilities. Realizing that many types of data can most easily be represented and manipulated by Lisp \(or Scheme\), and realizing that S-expressions would be the simplest way to represent the structure that HTML is used to represent, and learning some of the basics of JavaScript events, he hit upon the idea of using a direct manipulation interface to create and edit web pages.

Later the author discovered a version of Scheme running in, or on top of, JavaScript. As some programmers familiar with the Scheme programming language's `call/cc` form realized, the state of progression through a series of interactions can quickly grow overwhelming, but `call/cc` makes it possible to simplify the stateless web and allow it to be programmed in a stateful way.

The author had previously used the UCW \(UnCommon Web\) framework that used Common Lisp to create something like `call/cc` on the web server. His experience with UCW made him realize that a practical, generally useful web framework would be expensive and have limited scalability due to the memory consumed by users' simultaneously running web applications. He realized that by spreading the memory consumption across all the web browsers using a web app, that is, by storing the continuations in the web app's pages rather than on the web server, the web app could be scaled much higher.

A parallel motivation for building Zen is the author's dissatisfaction with web browsers, websites, and utilities for study in the world's new library, the web.

## Part 2: Goals and Possible Goals of Zen

Motivations and goals are not the same thing. When will Zen be complete? Or when will a _version_ of it really exist? The answer is: when its goals have been met.

In reality, though, Zen has a graded set of _possible_ goals, some imminently achievable and some just remotely believable. Part 2 will break this down.

## Part 3: The Key to Zen: A Nano Framework for Sequential Programming

Virtually every part of Zen involves intricately designed user interaction. This can be managed only if the asynchronous, stateless environment of JavaScript in the web page can be tamed and brought under the control of a sequential process. It is painful to say it, but it sometimes appears that most of today's web developers believe the most natural way to program user interactions is with pure event driven code. Even seasoned programmers appear to lose sight of the fact that an application that doesn't progress logically through a series of goals is just a random process. Turning complex user interactions into sequential processes by using `call/cc` is Zen's key to success.

## Part 4: Web Page Structure

## Part 5: User Interactions and the HTML5 APIs

## Part 6: Example Web Apps Built with Zen

