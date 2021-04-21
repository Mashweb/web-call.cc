---
description: >-
  The HTML APIs enable many kinds of drag and drop interactions such as dragging
  and dropping text, or links, or HTML, or files, or images, or nodes, or even
  custom data.
---

# HTML Drag and Drop, Including File Drag and Drop

The HTML Drag and Drop API supports many kinds of drag and drop. See ["Recommended Drag Types"](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/Recommended_drag_types) on MDN.

Web browsers use the same general API, [the HTML Drag and Drop API](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API), for dragging files onto a web page that they use to drag HTML elements in a web page, but they use [different parts of the API](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/File_drag_and_drop). The [living standard is here on whatwg.org](https://html.spec.whatwg.org/multipage/dnd.html), updated in April, 2021.

The [Zen live DOM editor demo](https://web-call.cc/) responds to file dragging and dropping, but crashes because it receives input that causes an unhandled exception, which can be seen if the browser's JavaScript console is opened.

[Demo \#4](https://web-call.cc/blog-app.html) on [web-call.cc](https://web-call.cc), a so-called "financial blog," has cleaner, simpler code to manage a drag and drop operation, but it is still buggy and it is not meant to handle file drag and drop. If one of the draggable elements is dragged and dropped outside the drop zone, the operation is cancelled and cannot be rebegun.

Note: drag and drop can be programmed without the special [HTML Drag and Drop API](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API), using just mouse events. There can, in some cases, be a good reason for this. See the article ["Drag'n'Drop with mouse events"](https://javascript.info/mouse-drag-and-drop) on [javascript.info](https://javascript.info).

Here is a list of basic HTML Drag and Drop API tutorials and how-tos:

1. ["Using the HTML5 Drag and Drop API"](https://web.dev/drag-and-drop/) on web.dev.
2. ["Creating a Parking Game With the HTML Drag and Drop API"](https://css-tricks.com/creating-a-parking-game-with-the-html-drag-and-drop-api/) on css-tricks.com.
3. ["Accessible Drag and Drop with Multiple Items"](https://www.sitepoint.com/accessible-drag-drop/) on sitepoint.com.
4. The old article ["The HTML5 drag and drop disaster"](https://www.quirksmode.org/blog/archives/2009/09/the_html5_drag.html) on quirksmode.org explains many tricky points of the API.
5. Here is [a version of drag and drop](https://www.quirksmode.org/js/dragdrop.html) that doesn't use the HTML Drag and Drop API, on quirksmode.org.
6. The very old article ["Beneath The Surface"](http://ln.hixie.ch/?start=1115899732) on ln.hixie.ch explains many stumbling blocks to interpreting the API.
7. The old article ["On HTML5 Drag and Drop"](http://tolmasky.com/2009/08/16/on-html-5-drag-and-drop/) on tolmasky.com introduces the HTML Drag and Drop API with an emphasis on how the API extends drag operations to the computer's native drag system and clipboard, thus allowing the user to share data between different web apps and even other desktop apps.



