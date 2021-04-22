# Zen User Intereactions

I envision that there will be various levels and classes of Zen user interaction. I will try to list them in this wiki page.

## Zen user interactions based upon hooking up HTML5 APIs to Zen's Scheme-in-the-browser

Note that [the full list of HTML5 APIs](https://developer.mozilla.org/en-US/docs/Web/API) is quite long, but below I've listed some that might interest us the most. I have left out APIs that don't work on the popular web browsers \(except for Explorer, which I ignore\) and APIs that don't work on Safari iOS \(with a view that Zen might someday soon work on touch screens\).

1. [Pointer events](https://developer.mozilla.org/en-US/docs/Web/API/Pointer_events) - see note \[1\] below
2. [Pointer Lock](https://developer.mozilla.org/en-US/docs/Web/API/Pointer_Lock_API) - see note \[2\] below
3. [HTML drag and drop](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API)
4. [File drag and drop](https://developer.mozilla.org/en-US/docs/Web/API/HTML_Drag_and_Drop_API/File_drag_and_drop)
5. [Clipboard](https://developer.mozilla.org/en-US/docs/Web/API/Clipboard_API)
6. [Intersection Observer API](https://developer.mozilla.org/en-US/docs/Web/API/Intersection_Observer_API) - well supported in browsers; see note \[3\] below
7. [Navigation Timing API](https://developer.mozilla.org/en-US/docs/Web/API/Navigation_timing_API)
8. [Gamepad](https://developer.mozilla.org/en-US/docs/Web/API/Gamepad_API) - not just for Xbox or PS, see [MacWorld article](https://www.macworld.co.uk/how-to/use-ps4-xbox-controller-mac-3626259/) or [CNet article](https://www.cnet.com/how-to/how-to-connect-ps4-xbox-one-controller-to-a-mac/)
9. [Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) - a bit like jQuery.ajax\(\), but better
10. [Screen Capture API](https://developer.mozilla.org/en-US/docs/Web/API/Screen_Capture_API) - well supported for desktop browsers
11. [Media Source API](https://developer.mozilla.org/en-US/docs/Web/API/Media_Source_Extensions_API)
12. [MediaStream Recording API](https://developer.mozilla.org/en-US/docs/Web/API/MediaStream_Recording_API)
13. [Canvas](https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API)
14. [WebGL](https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API)
15. [Channel messaging](https://developer.mozilla.org/en-US/docs/Web/API/Channel_Messaging_API)
16. [Contact picker](https://developer.mozilla.org/en-US/docs/Web/API/Contact_Picker_API)
17. [Long Tasks API](https://developer.mozilla.org/en-US/docs/Web/API/Long_Tasks_API) - not well supported by browsers, but might be useful for debugging
18. [MediaStream Image Capture API](https://developer.mozilla.org/en-US/docs/Web/API/MediaStream_Image_Capture_API) - experimental, not well supported by browsers
19. [Content index](https://developer.mozilla.org/en-US/docs/Web/API/Content_Index_API) - experimental, not well supported by browsers
20. [IndexedDB API](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API) - complicated API for Web Workers
21. [Encrypted media](https://developer.mozilla.org/en-US/docs/Web/API/Encrypted_Media_Extensions_API)
22. [File and directory entries](https://developer.mozilla.org/en-US/docs/Web/API/File_and_Directory_Entries_API) - don't use this until it is no longer marked experimental
23. [File System Access](https://developer.mozilla.org/en-US/docs/Web/API/File_System_Access_API) - browser compatibility unknown, but this page's "See also" link leads us to a [demo that works on Chrome \(see the section "Try it"\)](https://web.dev/file-system-access/)
24. [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API) - two-way interactive communication session between the user's browser and a server
25. [WebRTC API](https://developer.mozilla.org/en-US/docs/Web/API/WebRTC_API) - interesting

## Notes

1. > Much of today's web content assumes the user's pointing device will be a mouse. However, since many devices support other types of pointing input devices, such as pen/stylus and touch surfaces, extensions to the existing pointing device event models are needed. Pointer events address that need.
2. > The Pointer Lock API \(formerly called Mouse Lock API\) provides input methods based on the movement of the mouse over time \(i.e., deltas\), not just the absolute position of the mouse cursor in the viewport. It gives you access to raw mouse movement, locks the target of mouse events to a single element, eliminates limits on how far mouse movement can go in a single direction, and removes the cursor from view. It is ideal for first person 3D games, for example. More than that, the API is useful for any applications that require significant mouse input to control movements, rotate objects, and change entries, for example allowing users to control the viewing angle by moving the mouse around without any button clicking. The buttons are then freed up for other actions. Other examples include apps for viewing maps or satellite imagery.
3. > The Intersection Observer API provides a way to asynchronously observe changes in the intersection of a target element with an ancestor element or with a top-level document's viewport. Historically, detecting visibility of an element, or the relative visibility of two elements in relation to each other, has been a difficult task for which solutions have been unreliable and prone to causing the browser and the sites the user is accessing to become sluggish. As the web has matured, the need for this kind of information has grown. Intersection information is needed for many reasons, such as... .

## Zen's Adaptations of Raw HTML APIs

Sometimes it will be helpful to either \(1\) intimately interact with low-level HTML APIs like JavaScript.info's 

