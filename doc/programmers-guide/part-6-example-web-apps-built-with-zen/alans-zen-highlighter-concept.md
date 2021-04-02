# Alan's-Zen-Highlighter-Concept

## Highlighter Concept

Updates: 4/1/2021

Tom says: There are a lot of tasks embedded in building the Zen Highlighter, so let's break this down.

Let's put this into the [Guide to the Zen Web App Framework and Mashweb...](https://tomelam.gitbook.io/mashweb/).

## Key ideas

1. We are not stuck with the search capabilities of the document provider or the browser
2. We can search documents from multiple sources at the same time
3. We are interested in sentences or paragraphs, not just words
4. Search understands the DOM structure, not just plain text
5. Can augment with domain-specific search capabilities, e.g. chemistry, biology, computer science

Tom comments:

1. Good, but there is a Google operator "site:xyz.com these are my search terms", which most non-technical users probably don't know about. Still, the Zen Highlighter should be dog food for us, not for an undefined, un-"monetized" "crowd in the cloud". The Zen Highlighter should be something that _I_ would want to use \(mostly because I'm paying for it\).
2. See my point for key idea \#1.
3. Interesting, but how long would it take to program that? My budget is very limited.
4. How could this be used?
5. This sounds too difficult to implement without a lot of foregoing development. I want something useful very soon, based upon easy programming \(not visual programming such as [https://web-call.cc/visual-programming.html](https://web-call.cc/visual-programming.html)\).

## Scenario

1. Grab some web pages and drop into Zen Highlighter page
2. Web pages \(documents\)  are displayed as thumbnails with the active one differentiated from the others
3. Specify complex search criteria
   * Words and logical operators
   * Possibly with stemming and other capabilities behind the scenes
   * Hit the search button
4. Sentences in the documents that match the criteria are highlighted
5. Next/previous buttons to jump to highlighted sentences

## Operations on highlighted sentences

1. Can un-highlight uninteresting sentences
2. Can add the next or previous sentence to highlighting 
3. Can highlight sentences manually by clicking on a sentence
4. Can add a comment to a sentence
5. Can click one or more sentences and "get more like this" to expand the search

## Operations on documents

1. Can extract all highlighted sentences
2. Can do DOM operations like create an outline of the headings and highlight those that contain highlighted sentences
3. For documents that don't use headings, can construct effective outline based on DOM  structure
4. Can hide and unhide sections 
5. Can pull in linked documents and search them with same criteria

## Paragraphs

1. Can have all of the same capabilities for paragraphs instead of sentences \(and might even be easier\)
2. Can do DOM aware operations like get intro paragraph and/or closing paragraph for every section

## Where to go from here

Tom says:

The Dojo Toolkit might provide more fodder for ideas. See [https://github.com/Mashweb/web-call.cc/wiki/Zen-Widgets:-Their-Creation,-Manipulation,-and-Affordances](https://github.com/Mashweb/web-call.cc/wiki/Zen-Widgets:-Their-Creation,-Manipulation,-and-Affordances).

I have begun editing a wiki page that will help us adapt and adopt some of your ideas quickly: [Zen user intereactions](https://github.com/Mashweb/web-call.cc/wiki/Zen-user-intereactions).

Note that it is easy to "embed" videos in a Zen web page. \(See [how easy it is to "embed" a YouTube video in a GitHub wiki](https://stackoverflow.com/a/16079387), for example.\) That is actually something I would like to do, because I find prominent people are often interviewed for the news.

I will work on your ideas some more later today, Alan. You have come up with a lot of good ideas. I don't want to shoot them down. Let's see how we can develop them quickly so they can help tech savvy people like us. ;-\) -Tom

