# Zen Structure Editor

## The theory

This is the theory of how web page editing will work in Zen. I'm not sure this will work, but I want to try to make it work using just a few simple methods to solve or work around big problems of web page construction and styling. The theory boils down to two ideas:

1. A Zen web page could most easily be built "declaratively" using only HTML elements and Custom Elements. This simplification makes the choice of a backend web framework irrelevant to the concerns of Zen's web page editor and encapsulates all JavaScript related to Custom Elements so that a user of Zen won't have to touch JavaScript.
2. The web page editing can be separated into two phases: _structure editing_ and _semantics editing_.

The user should be able to add elements from a reasonably large and inclusive set of non deprecated HTML elements and Custom Elements. However, in choosing elements to place on his web page to begin seeing his page take shape, he should not get bogged down with the details of a vast set of options. Instead, he should be able to set up a top-level structure first: the look of the page. This will give him an overview of his work or play and serve him as a management and planning tool. Zen's web page editor would have a mode called _Structure Mode_.

Good web design demands that the parts of a web page's structure be chosen semantically. This might help [search engine optimization \(SEO\)](https://searchengineland.com/guide/what-is-seo) and [web accessibility](https://www.w3.org/WAI/fundamentals/accessibility-intro/). Zen's web page editor would have a mode called _Semantics Mode_ for choosing HTML elements and Custom Elements appropriately for SEO and accessibility.

## Structure Mode

The intent of _Structure Mode_ is that in it only `<DIV>` elements, representative of any HTML element or Custom Element, will be added, moved, deleted, sized and various of their attributes will be set. Those attributes could be ones that affect the look of a web page, such as height, width, foreground and background colors, background image. The theory behind _Structure Mode_ is that there are really only two basic plans for defining the layout of any branch of a web page's structure: block-level layout and inline layout. For an explanation of this, see ["HTML Content Models"](https://clearlydecoded.com/html-content-models) on [clearlydecoded.com](https://clearlydecoded.com/) and for a more complete background, see ["Content categories"](https://developer.mozilla.org/en-US/docs/Web/Guide/HTML/Content_categories) on [MDN](https://developer.mozilla.org/en-US/) and ["Content models"](https://www.w3.org/TR/2011/WD-html5-20110525/content-models.html) on [w3.org](https://www.w3.org/).

## Semantics Mode

After a user sets the look of his web page using the _Structure Mode_, he should want to make the page SEO-friendly and accessible. He can do that by replacing &lt;DIV&gt; elements in his page with semantically appropriate elements using Zen's _Semantics Mode_.

## Questions

Some, perhaps many, HTML elements, such as `<LI>` \(the element that defines an item in a numbered list\), have a unique appearance that might not be reproducible using just `<DIV>` elements. I don't know yet whether this problem will make Zen's modal editing of web pages unworkable.

Tom Elam, April 24, 2021

