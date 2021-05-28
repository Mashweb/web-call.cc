---
description: >-
  This part presents some concepts related to sequential programming that are
  key to Zen.
---

# Part 3: The Key to Zen: A Nano Framework for Sequential Programming

Virtually every part of Zen involves intricately designed user interaction. This can be managed only if the asynchronous, stateless environment of JavaScript in the web page can be tamed and brought under the control of a sequential process. It is painful to say it, but it sometimes appears that most of today's web developers believe the most natural way to program user interactions is with pure event driven code. Even seasoned programmers appear to lose sight of the fact that an application that doesn't progress logically through a series of goals is just a random process. Turning complex user interactions into sequential processes by using `call/cc` is Zen's key to success.

The page [Sequentially Programmed Web App Demo](https://doc.mashweb.club/experiments/seq_webapp_biwascheme/) on [Mashweb.Club](https://mashweb.club/) goes into depth on the topics of web continuations and the sequential programming for web apps. The topics there _must_ be understood by a programmer who wants to create web apps using Zen. The page [Demo \#2: Introduction to Zen sequential web app programming](https://web-call.cc/sequentially-programmed-web-apps.html) presents the same demo and one more, a calculator web app, whose body is just 18 lines. The brevity of such an app demonstrates the simplicity and straightforwardness of Zen web apps.

The way Zen uses `call/cc` is reminiscent of how the POSIX system calls [`select(2)`](https://linux.die.net/man/2/select) and [`poll(2)`](https://linux.die.net/man/2/poll) work. Here is a [Biwascheme](https://www.biwascheme.org/) code block using Zen's `with-handlers` macro:

```scheme
(with-handlers ((click-handler "#div1")
                (click-handler "#div2")
                (keydown-handler "#button1")
                (keydown-handler "#button2")
                (timeout-handler test-timeout 10000))
  (display (get-input))
  (display (get-input))
  (display (get-input))
  (display (get-input))
  (display (get-input)))
```

`with-handlers` defines a code block wherein JavaScript event handlers are set up. When execution exits the code block, the event handlers are automatically torn down. Each call to `get-input` retrieves all the data returned by one of the event handlers, in the order in which the events occur. The plan for Zen is to make [synthetic JavaScript events](https://developer.mozilla.org/en-US/docs/Web/Events/Creating_and_triggering_events) interwork with native JavaScript events to simplify control flow in complex user interactions. This way, multiple events of a type such as keydown could be bundled to trigger an event and be handled with the same priority as events such as the pressing of a button.  For more details about `with-handlers`, see [its implementation](https://raw.githubusercontent.com/Mashweb/web-call.cc/master/source/scheme/mini-framework.scm).

