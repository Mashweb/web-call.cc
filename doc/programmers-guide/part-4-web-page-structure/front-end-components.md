---
description: >-
  This page lists some worthwhile online resources on web input elements and
  widgets and other front-end components.
---

# Front-End Components

## Vaadin resources

Here is a random list \(in no particular order\) of Vaadin resources on web form input elements, interaction components, and widgets that I found interesting:

1. [https://vaadin.com/learn/tutorials/vaadin-key-concepts](https://vaadin.com/learn/tutorials/vaadin-key-concepts)
2. [https://vaadin.com/directory/](https://vaadin.com/directory/) This is a searchable Vaadin component directory. To find out how to use a component declaratively in an HTML page, select a component, then click on the "API" or "API documentation" link, then look for the HTML API. Some components don't appear to have an HTML API, so perhaps they can't be used declaratively. The `vaadin-details` component _does_ have an HTML API, so it can be used as a reference for how to look up an HTML API.
3. [https://vaadin.com/learn/tutorials/using-web-components](https://vaadin.com/learn/tutorials/using-web-components)
4. [vaadin.com/components](https://vaadin.com/components)
5. [https://vaadin.com/docs/latest/ds/components](https://vaadin.com/docs/latest/ds/components)
6. [https://vaadin.c](https://vaadin.com/learn/tutorials)[om/learn/tutorials](https://vaadin.com/learn/tutorials)
7. [https://vaadin.com/docs/v8/framework/application/application-declarative](https://vaadin.com/docs/v8/framework/application/application-declarative)

The following are listed to round out this overview of the Vaadin framework:

1. [https://vaadin.com/blog/free-fact-sheets-on-vaadin-and-features](https://vaadin.com/blog/free-fact-sheets-on-vaadin-and-features)
2. [https://v.vaadin.com/hubfs/Pdfs/Vaadin-components-fact-sheet.pdf](https://v.vaadin.com/hubfs/Pdfs/Vaadin-components-fact-sheet.pdf)
3. [https://vaadin.com/docs/v8/framework/application/application-declarative](https://vaadin.com/docs/v8/framework/application/application-declarative)
4. [https://vaadin.com/learn/training/client-side-introduction](https://vaadin.com/learn/training/client-side-introduction)
5. [https://vaadin.com/docs/v8/framework/architecture/architecture-client-side](https://vaadin.com/docs/v8/framework/architecture/architecture-client-side)
6. [https://vaadin.com/docs/v8/framework/clientside/clientside-overview](https://vaadin.com/docs/v8/framework/clientside/clientside-overview)
7. [https://vaadin.com/docs/v8/framework/clientsidewidgets/clientsidewidgets-vaadin](https://vaadin.com/docs/v8/framework/clientsidewidgets/clientsidewidgets-vaadin)
8. [https://vaadin.com/directory/component/htmlelementssmart-custom-element](https://vaadin.com/directory/component/htmlelementssmart-custom-element)

## Dojo Toolkit resources

My disappointment with [Dojo 7](https://dojo.io/) \(and [possibly all versions of Dojo later than version 1.x](https://dojo.io/blog/dojo2-0-0-release)\) was that I found that examples of dragging and dropping an HTML element could not be extended to Dojo 7's widgets properly. Those examples do not work for Dojo 7 widgets because the element that a `dragstart` event actually targets when the mouse is clicked over a Dojo 7 widget is not the outer HTML element, but its child. Perhaps the older version of Dojo, called [the Dojo Toolkit](https://dojotoolkit.org/) will not have this problem. I have not tested it with my [live DOM editor](https://web-call.cc/) or my [refactored drag and drop code](https://web-call.cc/blog-app.html).



