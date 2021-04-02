---
description: >-
  This part presents some concepts related to sequential programming that are
  key to Zen.
---

# Part 3: The Key to Zen: A Nano Framework for Sequential Programming

Virtually every part of Zen involves intricately designed user interaction. This can be managed only if the asynchronous, stateless environment of JavaScript in the web page can be tamed and brought under the control of a sequential process. It is painful to say it, but it sometimes appears that most of today's web developers believe the most natural way to program user interactions is with pure event driven code. Even seasoned programmers appear to lose sight of the fact that an application that doesn't progress logically through a series of goals is just a random process. Turning complex user interactions into sequential processes by using `call/cc` is Zen's key to success.

The page [Sequentially Programmed Web App Demo](https://doc.mashweb.club/experiments/seq_webapp_biwascheme/) on Mashweb.Club goes into depth on the topics of web continuations and the sequential programming for web apps. The topics there _must_ be understood by a programmer who wants to create web apps using Zen. The page [Demo \#2: Introduction to Zen sequential web app programming](https://web-call.cc/sequentially-programmed-web-apps.html) presents the same demo and one more, a calculator web app, whose body is just 18 lines. The brevity of such an app demonstrates the simplicity and straightforwardness of Zen web apps.

