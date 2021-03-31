# Zen-elevator-pitches

Please refer to [web-call.cc](https://web-call.cc/) and [Mashweb.Club](https://mashweb.club/) for more on Zen.

## 90-second pitch \(?\)

"I really need a PR person to write up some articles for me, but I'll try to be my own PR guy for now. I think there is no need for pessimism about technology. There is always a new generation coming up with a fresh gestalt based upon the faint but growing signals in the memosphere that only their young, sharp ears can pick up. Large corporations have no advantage in the field of ideas. I think that what I'm working on has some of the elements of the future in it. Judge for yourself.

"As I say in my white paper, 'Since, with Zen, deploying and sharing a new application could be as simple as clicking a Share or Deploy button, self-organizing user communities might develop altogether novel applications.' Remember, some people prefer exclusivity to ubiquity. This is how communities like the Well, Clubhouse, and Parler gained some limited but influential popularity, still survive, still grow, and gradually become more influential.

"What I have discovered is that it is extremely easy \(relative to callbacks, promises, async/await programming\) to program user interfaces sequentially. I have created a mini framework that comprises an interpreter running inside of \(or on top of\) JavaScript that implements a flexible control operator \(actual syntax\) to support event-driven programming. My early experiments using this control operator can be found on [https://web-call.cc](https://web-call.cc). Additional background for understanding sequential web programming is on this page on Mashweb.Club.

"Typical modern user interactions are most likely impoverished because it is very difficult and expensive to create intricate interactions with many desirable affordances. The complexity will be unmanageable if event handling is programmed with callbacks, promises, and async/await, without anything more like the human processes that they must emulate."

## Linkedin connection pitch

Here's a workable note added to a Linkedin connection request: "**\_\_**, we have some similar tech interests. I have created a web framework that uses an interpreter running on top of JavaScript to implement a flexible control operator \(syntax\) for sequential event-driven programming, without callbacks, promises, or async/wait. Have a look: [https://bit.ly/3lV1knf](https://bit.ly/3lV1knf)"

## ACM SIGPLAN "pitch"

Please compare and contrast my elevator pitch with [the article on Whalesong published the ACM SIGPLAN Notices](https://dl.acm.org/doi/abs/10.1145/2578856.2508172), whose abstract is:

> JavaScript is the language of the ubiquitous Web, but it only poorly supports event-driven functional programs due to its single-threaded, asynchronous nature and lack of rich control flow operators. We present Whalesong, a compiler from Racket that generates JavaScript code that masks these problems. We discuss the implementation strategy using delimited continuations, an interface to the DOM, and an FFI for adapting JavaScript libraries to add new platform-dependent reactive features. In the process, we also describe extensions to Racket's functional event-driven programming model. We also briefly discuss the implementation details.

\(The full article is available online [here](https://cs.brown.edu/~sk/Publications/Papers/Published/yk-whalesong-racket-browser/paper.pdf).\)

## Criticism

We should be well cognizant of newer features of JavaScript \(well presented in MDN's series of articles beginning with [Introducing asynchronous JavaScript](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Introducing)\) and be able to defend the use of call/cc in the web browser.

