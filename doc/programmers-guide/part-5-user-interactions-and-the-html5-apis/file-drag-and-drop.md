# File Drag and Drop

Web browsers use the same general API, [the HTML Drag and Drop API](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API), for dragging files onto a web page that they use to drag HTML elements in a web page, but they use [different parts of the API](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/File_drag_and_drop).

The [Zen live DOM editor demo](https://web-call.cc/) responds to file dragging and dropping, but crashes because it receives input that causes an unhandled exception, which can be seen if the browser's JavaScript console is opened.

