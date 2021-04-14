---
description: This page was first posted on tomelam.blogspot.com about 2010.
---

# Zen: First Principles

 This is the second in a series of articles on Zen, that began with the article ['Zen Project \#1: A Web Site and a New Technology to Help People Scale the Web'](http://tomelam.blogspot.com/2010/09/project-web-site-and-new-technology-to.html). Technical aspects of Zen are described at [www.tomelam.com](http://www.tomelam.com/).

 Zen has ambitious goals. The key to its success will lie in whether it remains true to its unique set of first principles:

1. Zen is global, i.e. it exists on the Web.
2. Zen is a zero-install program, i.e. it provides its basic features without browser plug-ins or extensions.
3. Zen will provide [graphical user interfaces](http://en.wikipedia.org/wiki/Graphical_user_interface) for adding many kinds of [widgets](http://en.wikipedia.org/wiki/GUI_widget) provided by HTML and [JavaScript frameworks](http://en.wikipedia.org/wiki/Comparison_of_JavaScript_frameworks) to a Zen web page. Of course, if too many frameworks—or the wrong combination of frameworks—are used in a single web page, trouble ensues. Zen does not in general impose any limitations upon the way the widgets and frameworks can be combined, but there is a very safe and easy-to-use version of Zen for general use by all users.
4. Zen will "engulf the Web": it will provide a user-programmable widget that will embed a "web browser" in a Zen web page. Again, even non-technical users will be able to "program" this web browser to do such things as make any web page dissectible, editable, rearrangeable, and [mash-able](http://www.programmableweb.com/faq#Q1). It will be possible to embed multiple copies of the "browser widget" in a web page. The user will be able to use Zen to copy or move widgets and components from the embedded web page into the rest of the Zen web page. This principle of "engulfing the Web" is very difficult to remain true to because it implies that web pages will be analyzed by [Zen's special web server](http://tomelam.blogspot.com/2010/09/project-web-site-and-new-technology-to.html#proxy1), which has proxy capabilities and the ability to internally render JavaScript-enabled web pages. The special web server will also allow [any page to be hot-linked](http://tomelam.blogspot.com/2010/09/project-web-site-and-new-technology-to.html#proxy2). \[March 15, 2012: As of now, clicking on the "any page to be hot-linked" link only leads to the top of the blog post. Please skip down to the last paragraph of the post. Something about Blogger.com seems to have prevented a link target inside a post to be accessed.\] The special web server will also emulate referral links, thereby allowing a web document that can only be accessed through a referral link \(not directly via URL\) to be accessed from any Zen-enabled web page.
5. Zen provides many functions like filter and sort that even non-technical users can apply to data sources just by dragging widgets around in the web page. It will be possible to chain many of the functions together to provide more possibilities for data manipulation.
6. A user will be able to capture a sequence of interactions with their Zen web page such as moving the mouse pointer over a particular kind of widget \(the technicalities of what "kind" means here are not very important in this discussion\), clicking on a widget or web page component, typing a single character on the keyboard, entering a string of characters in a box, etc. The range of interaction events that can be captured is from the lowest level interaction with a web page \(characters and mouse interactions including the clicking upon submit buttons\) through any kind of widget interaction. The user can generalize the captured sequence \("parameterize" it\) so that it can be applied to a class of situations. He can store it and attach it to a menu or button so that it can be evoked by choosing the menu item or clicking the button. Thereafter when the user of the page containing the parameterized sequence interacts with the page his interactions with the page are circumscribed: his out-of-sequence input is optionally treated as an error and he can be gently guided to follow the sequence. The capabilities of this Zen sequence capture are somewhat elaborated from the basics described here.
7. A user will be able to create persistent copies of his Zen web pages. These copies will be saved on the Zen web server.
8. Zen will facilitate a user creating web pages and web applications that do not include the Zen library. After Zen removes itself from these pages and applications, they will only depend upon HTML, one or more already-well-accepted JavaScript libraries, and a minimum of JavaScript "glue." In another way of speaking, Zen will export such pages and applications. Some part of the Zen library will be required to enable all features of captured sequences, however.
9. Zen can be augmented with features that break its first principles, but the result will not be Zen.
10. As an exception to First Principle \#1, Zen can be injected into any web page, via web browser add-ons or extensions, to allow the page to be redesigned and mashed up with other content and to gain Zen features. However, only content managed by cooperative JavaScript libraries will be fully compatible with Zen. A cooperative library must facilitate the tracking of every widget and every "DOM element" that the library adds to or subtracts from the web page. \(DOM elements are the "atoms" or structural components of a web page. Tables, divisions of a page, paragraphs, lists, and list items, among other things, are represented by DOM elements.\) Furthermore, a cooperative library must facilitate the tracking of actions that it can perform in response to the user's interaction with the web page or in response to data received from the web server. \(Such prepared actions are called "event handlers." They respond to interaction such as mouse clicking and key pressing.\) The word facilitate is a bit ambiguous: it can mean "make it possible" or "make it easy." So far as Zen is concerned, a cooperative library should only have to make possible the things just mentioned, but in its first iterations, Zen might only be fully compatible with libraries that make those things easy.

 I might add more first principles to this list later.

 I am still seeking collaborations from companies, investors, technology marketing experts, and programmers. Please post your comments.
