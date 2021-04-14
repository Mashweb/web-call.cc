---
description: >-
  This page was first posted on tomelam.blogspot.com about 2010 and lightly
  edited in 2018.
---

# Zen Project \#1: A Web Site and a New Technology to Help People Scale the Web

| [![](https://farm4.staticflickr.com/3704/12325442215_26f544d076_h.jpg)](https://farm4.staticflickr.com/3704/12325442215_26f544d076_h.jpg) |
| :--- |
| Ascending and Descending, M. C. Escher, 1960 |

In this, the first in a series of articles on Zen, I describe a large and general problem people have as users of the Web. Then I describe a set of technologies I am working on, more fully described at [Mashweb.Club](https://mashweb.club/) and my older website, [tomelam.com](http://www.tomelam.com/), which I call Zen, that could significantly reduce the problem. On average, each of us consumes three times as much information as we did in 1960 and checks 40 web sites a day, according to a [2010 article](http://www.npr.org/templates/story/story.php?storyId=129384107) on National Public Radio's web site. [New York Times articles](http://topics.nytimes.com/top/features/timestopics/series/your_brain_on_computers/index.html) tell us that this makes us impatient, forgetful, anxious, shallow, and unfocused. Information is our crack cocaine, and we should have been in rehab long ago. An information fix is even called a hit! \(See below.\)

Part of the problem is the difficulty in finding what we seek—even when we know it is on the Web. When we search for something on the Web, we hope that the process will be linear and finite, but we get lost in a [strange loop](http://en.wikipedia.org/wiki/Strange_loop), a [tangled hierarchy](https://en.wikipedia.org/w/index.php?title=Tangled_hierarchy&redirect=no) of searching, scanning, surfing, and subscribing that we try to climb down like a staircase. In our search for digital nirvana, employing bookmarks, tags, annotations, hyperlinks, search, indices, rankings, ratings, and subscriptions, we find ourselves skipping up and down between various levels of metadata and web resources, like the monks trudging endlessly on the paradoxical, looped staircase in Escher's lithograph "Ascending and Descending," shown above. On our way we collect scores of bookmarks and piles of paper notes, and get a headache and short temper, too.

Let us try to outline the stages or hierarchies of the most general search:

1. Search for an answer to a question using a general purpose or specialized "search engine," typically by entering keywords, phrases, and parameters \(e.g., date, tags, domain, URL, links-to, similar-to\). Items that match the query criteria are called hits. Hits are presented in lists or in hierarchical outlines and are typically ordered by criteria such as relevance or paid placement. Searching is optional if we can replace the list or outline of hits with a list or outline of resources from categories in a Web directory. Examples of [directories of the whole Web](http://en.wikipedia.org/wiki/Web_directory) are [Yahoo! Directory](https://searchengineland.com/library/yahoo/yahoo-directory) and the [Open Directory Project](https://en.wikipedia.org/wiki/DMOZ). Many specialized directories of web resources also exist.
2. Examine brief descriptions, summaries, or snippets of the items in the list or outline, if available.
3. Open the links to the relevant-looking hits—in new windows or tabs if possible. Evaluate these linked-to resources for fitness to our purpose.

This linear procedure can often miss highly specialized information. Sometimes this happens because we weren't able to choose the best search query at the outset: we didn't have enough information to choose well. Sometimes we might not even know information is available pertaining to our problem. And sometimes we really need information from the "deep web," i.e., data available only behind a form-oriented user interface, such as an airline fare search interface or a phone directory search interface. The "deep web" is generally not indexable by web search engines \("spiders" to be more precise\). A better alternative to this hit-and-miss search technique is "idea search," also called "idea navigation." \(See [Google's idea search](http://library.fora.tv/fora/showthread.php?t=12526) and [Stewart, Scott, and Zelevinsky's research](https://docs.google.com/viewer?url=http://www.robinstewart.com/research/papers/stewart08ideanav.pdf&pli=1).\) The problem with idea navigation is that it is only in its research phase of existence.

We often resort to trial-and-error variations of our search, but this is usually frustrating. A better approach than the trial-and-error, hit-or-miss approach is to refine each level of hierarchy, as described above, by applying the whole search procedure to that level. That is, we seek information resources, meta-resources \(e.g., search engines\), and methods of search. A method of search can involve subscription to a forum, wiki, mailing list, social bookmarking service \(like [Delicious](https://del.icio.us/), [Digg](http://digg.com/), and [reddit](http://www.reddit.com/)\), or [social media](http://en.wikipedia.org/wiki/Social_media) so that we can make new friends and acquaintances \(or communicate with old friends\) and get help from them. Or it might be to subscribe to pertinent blogs in an attempt to find out which experts are sharing helpful information on our topic. \(See [Google Reader](http://reader.google.com/) and [Google Blog Search](https://en.wikipedia.org/wiki/Google_Blog_Search).\) The "helpful information" could be the final solution we are seeking, or it could be a set of new keywords we hadn't thought of or known, or it could be some other forum, wiki, blog, mailing list, chat room, or other resource. The results on topic-specific forums, wikis, blogs, mailing lists, and chat rooms will be pre-filtered by design, avoiding completely irrelevant hits that can swamp the relevant ones in a general-purpose whole-Web search engine. There are many other strategies for refining the levels of search hierarchy.

Other problems arise in our research on the Web and in remembering what we find: too many web pages and no adequate means of keeping track of them. If we open a window for each web page that looks pertinent to our search, we can soon overburden our web browser or our operating system. The browser can crash, or, in the case of Google Chrome, a tab pane can crash. Web browsing can slow to a desperate crawl. If we bookmark our web pages, we lose time navigating the browser's bookmark manager and might not be able to find the bookmarks later anyway.

So what's the answer, prior to the advent of useful "idea browsing"? I propose we make it possible to write the Web as easily as desktop text documents can be written, and to implement some of the original ideas about hypertext like [transclusion](http://en.wikipedia.org/wiki/Transclusion), so one single Web page becomes our notebook and scrapbook. The reader might think of [Evernote](http://www.evernote.com/) or [Google Notebook](https://www.google.com/notebook), but the kernel of the technology I'm working on focuses on organizing and tuning the user's view of and interaction with the Web and on being universally usable, globally, by anyone with a reasonably up-to-date web browser, without browser plug-ins. \(I will even try to support Internet Explorer 6.\) On top of the kernel, many web-server-assisted capabilities can be built.

Zen will allow the user to get close to the process of refining his searches, so that he can keep meta-resources, final results, and records of how he got results at his fingertips, under his control, "programmed" via a simple visual interface. He should be able to "program the Internet" by dragging and flowing links, text, data in [microformats](http://en.wikipedia.org/wiki/Microformat), and media from any web page into a web page he controls and saves to a web server. He should be able to use those links as bookmarks. He should be able to organize the links, text, and media in folders, tab panes, accordion panes, and other web widgets and lazy-load them so that time is not taken when he re-opens his page at a later date. Only the open pages or other widgets that contain the links, text, data, and media should be loaded. \(That's how a lazy-loaded pane works.\) Zen will even allow lazy-loaded widgets to be put into other lazy-loaded widgets. Many of the web-server-assisted services mentioned above and below can easily be codified and automated so that they can be included as widgets on a web page.

The user will be able to share these pages over the Web with other people very easily, in many ways. The special web server will serve his web page and will use its in-built proxy capabilities to enable the user's web page to be composed of content from multiple web sites. Such a composition is called a [mashup](http://en.wikipedia.org/wiki/Mashup_%28web_application_hybrid%29). The JavaScript inside the user's web page will enable many kinds of mashup to be created even without help from the special web server. These JavaScript-enabled mashups will use certain kinds of mashable content from multiple web sites. Microformats, mentioned above, can assist the mashups to "digest" and pour content from disparate places into the page. For example, much [social media](http://en.wikipedia.org/wiki/Social_media), including the present blog, can be accessed via [web feeds](http://en.wikipedia.org/wiki/Web_feed) using an [RSS](http://en.wikipedia.org/wiki/RSS) or [Atom](http://en.wikipedia.org/wiki/Atom_%28standard%29) microformat. This allows the media to be formatted by a [news aggregator](http://en.wikipedia.org/wiki/News_aggregator) for human consumption or inserted into a web page.

Various features of Zen will allow development of a web page to proceed smoothly. The special web server will also enable the user's Web navigation to be recorded automatically. Variations of the user's web page will be as easily produced as [Git branches](http://www.kernel.org/pub/software/scm/git/docs/gittutorial.html), because a Git-like "filesystem" on the web server will record the components of the web page analogously to Git commits. The system must be easy to understand without much training.

I am beginning [to program Zen](http://tomelam.blogspot.com/2011/07/zen-rails-embedded-version.html) to enable this writable Web. Some description of the technology and some videos of my earliest prototypes of the technology are available at [www.tomelam.com](http://www.tomelam.com/). Eventually, I will create a web site offering registrations for people to create their own portals to the Web. These will be real portals: most of the media content in the web pages will be [hot-linked](http://en.wikipedia.org/wiki/Hotlink), not copied. For web sites that disallow hot-linking, the special web proxy will be used to work around the limitation by imitating web browsers that such web sites expect to deal with. I am hoping that many people will be interested in the technology, and I am seeking collaborations from companies, investors, technology marketing experts, and programmers. Please post your comments.
