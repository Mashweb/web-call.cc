# Zen Widgets: Their Creation, Manipulation, and Affordances

Around 2007, it was possible to create rich web apps using the Dojo Toolkit \(not to be confused with the Dojo reboot post Dojo 1.x\). See [dojotoolkit.org](https://dojotoolkit.org/). See the [Dojo Toolkit Demo Index](http://demos.dojotoolkit.org/demos/). The widgets that Dojo Toolkit provides can probably easily be added to a Zen web page, cloned, placed, moved around, etc. The author has used these widgets. They provide a lot of sophistication to a web app without slowing down the loading of pages. We will try to use them to replace some or all of the Vaadin Custom Elements on [web-call.cc](https://web-call.cc) because they seem to load much quicker than the Vaadin Custom Elements the author has tried.

Hopefully the Dojo Toolkit widgets will be easier to drag than [the new Web Component based Dojo widgets](https://dojo.io/) \(at version 7 as of June 1, 2020\). The function [findTrueTarget](https://github.com/Mashweb/web-call.cc/blob/master/source/javascripts/in-browser-gui.js) used in the [Zen live DOM editor demo](https://web-call.cc/) determines whether a [dragstart event](https://developer.mozilla.org/en-US/docs/Web/API/Document/dragstart_event) target is the top node of a multi-node Web Component. It is needed for dragging a new Dojo widget because the dragstart target is not the top node of the widget. The function would be unnecessary if the demo only included HTML elements and Vaadin Web Components, because the dragstart event target _is always_ the top node of those.

The following Dojo Toolkit widgets might be useful for creating Zen web apps:

1. [BorderContainer](https://dojotoolkit.org/documentation/tutorials/1.10/dijit_layout/demo/nestedBorderContainer.html)
2. [TabContainer](https://dojotoolkit.org/documentation/tutorials/1.10/dijit_layout/demo/appLayout.html)
3. [StackContainer](https://dojotoolkit.org/documentation/tutorials/1.10/dijit_layout/demo/stackContainerAppLayout.html)
4. [many others](https://dojotoolkit.org/documentation/#widgets) described in the documentation

Here are some complex examples to show what Dojo Toolkit widgets can do:

1. [An enhanced grid](http://demos.dojotoolkit.org/demos/grid/demo.html)
2. [A mockup of a complex web app](http://demos.dojotoolkit.org/demos/mail/demo.html)
3. [CSS3 animation](http://demos.dojotoolkit.org/demos/css3/demo.html)

This page will be added to as the author has time.



