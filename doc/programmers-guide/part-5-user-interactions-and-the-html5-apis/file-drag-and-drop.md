---
description: >-
  The HTML APIs enable many kinds of drag and drop interactions such as dragging
  and dropping text, or links, or HTML, or files, or images, or nodes, or even
  custom data.
---

# HTML Drag and Drop, Including File Drag and Drop

The HTML Drag and Drop API supports many kinds of drag and drop. See ["Recommended Drag Types"](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/Recommended_drag_types) on MDN.

Web browsers use the same general API, [the HTML Drag and Drop API](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API), for dragging files onto a web page that they use to drag HTML elements in a web page, but they use [different parts of the API](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/File_drag_and_drop).

The [Zen live DOM editor demo](https://web-call.cc/) responds to file dragging and dropping, but crashes because it receives input that causes an unhandled exception, which can be seen if the browser's JavaScript console is opened.

[Demo \#4](https://web-call.cc/blog-app.html) on [web-call.cc](https://web-call.cc), a so-called "financial blog," has cleaner, simpler code to manage a drag and drop operation, but it is still buggy and it is not meant to handle file drag and drop. If one of the draggable elements is dragged and dropped outside the drop zone, the operation is cancelled and cannot be rebegun.

Note: drag and drop can be programmed without the special [HTML Drag and Drop API](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API), using just mouse events. There can, in some cases, be a good reason for this. See the article ["Drag'n'Drop with mouse events"](https://javascript.info/mouse-drag-and-drop) on [javascript.info](https://javascript.info).



