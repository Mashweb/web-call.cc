// The value argument should be a string, either "true" or "false".
setDraggability = function (node, value) {
    var treeWalker = document.createTreeWalker(node, NodeFilter.SHOW_ELEMENT, {
        acceptNode: function (node) {
            return NodeFilter.FILTER_ACCEPT;
        }
    }, false);
    //console.log("TreeWalker created.");
    do {
        //console.log(treeWalker.currentNode);
        // Text selections, images, and links have an OS-dependent default rendering
        // of them being dragged. If we want that default behavior, we should not
        // set the "draggable" attribute of them.
	if (value == "true") {
            treeWalker.currentNode.draggable = value;
	} else {
	    treeWalker.currentNode.removeAttribute("draggable");
	}
    } while (treeWalker.nextNode());
    node.removeAttribute("draggable"); // Consider node to be only a container of draggables.
}

// cloneDOM

// Given an HTML element or custom element, clone it and its
// contents. Remove id attributes. Some custom elements,
// such as Dojo widgets, are not wholly draggable, so that
// when the user clicks on one of them, the target of the dragstart
// event does not contain the whole widget. That is why the function
// findTrueTarget is called.
cloneDOM = function(nodeToBeCloned) {
    var newTree, customTagName, childNodes, customElement;
    console.group("cloneDOM: nodeToBeCloned");
    console.dir(nodeToBeCloned);
    console.groupEnd();
    customElement = findTrueTarget(nodeToBeCloned);
    console.group("cloneDOM: customElement");
    console.dir(customElement);
    console.groupEnd();
    if (customElement) {
        console.log("cloneDOM: detected custom element; now call elementToTagName");
        customTagName = elementToTagName(customElement);
        console.log("cloneDOM: tagName (of custom element) => " + customTagName);
        newTree = document.createElement(customTagName, { is : customTagName });
        console.group("cloneDOM: newTree");
        console.dir(newTree);
        console.groupEnd();
        newTree.className = findTrueTarget(nodeToBeCloned).className; // TODO: Test this on a Dojo button.
        if (isComplexCustomElement(customTagName)) {
            console.log("complex custom element");
            childNodes = Array.from(customElement.childNodes[0].childNodes);
            //childNodes = [];
        } else {
            console.log("simple custom element");
            childNodes = Array.from(customElement.childNodes);
        }
        //console.log("cloneDOM: childNodes.length => " + childNodes.length);
        //console.group("cloneDOM custom element childNodes");
        //console.dir(childNodes);
        //console.groupEnd();
    } else {
        newTree = nodeToBeCloned.cloneNode(false);
        childNodes = Array.from(nodeToBeCloned.childNodes);
    }
    if (newTree.nodeType == Node.ELEMENT_NODE) {
        newTree.removeAttribute("id");
    }
    console.log("cloneDOM: childNodes.length => " + childNodes.length);
    console.group("cloneDOM: childNodes");
    console.dir(childNodes);
    console.groupEnd();
    subTrees = childNodes.map(function (node) {
        console.group("cloneDOM: childNodes.map appendChild's clone: node");
        console.dir(node);
        console.groupEnd();
        clone = cloneDOM(node);
        newTree.appendChild(clone);
    });
    console.group("cloneDOM: returning newTree");
    console.dir(newTree);
    console.groupEnd();
    return newTree;
}

elementToTagName = function(element) {    
    const regex = /[ >]/;
    console.group("elementToTagName: element");
    console.dir(element);
    console.groupEnd();
    //return element.parentElement.outerHTML.split(regex)[0].slice(1);
    console.log("elementToTagName: returning " + element.outerHTML.split(regex)[0].slice(1));
    return element.outerHTML.split(regex)[0].slice(1);
}

isComplexCustomElement = function(tagName) {
    console.log("isComplexCustomElement: tagName => " + tagName);
    return tagName.slice(0, 5) == "dojo-" ? true : false;
}

// The function findTrueTarget determines whether a dragstart event target points
// to a part of a custom element and, if it does, returns the top node of it.
// If the dragstart event target DOES NOT point to a part of a custom element,
// this function returns false.
// TODO: Add checks for other custom elements, as found necessary, like the
// check for Dojo custom elements here.
findTrueTarget = function(node) {
    // Look for a hyphen in the tag name
    var tagName, hyphenPos;
    if (node.nodeType != Node.ELEMENT_NODE) {
        return false;
    }
    console.log("findTrueTarget: call elementToTagName");
    //console.log("node.outerHTML => " + node.outerHTML);
    //console.log("node.innerHTML => " + node.innerHTML);
    tagName = elementToTagName(node);
    console.log("findTrueTarget: tagName => " + tagName);
    hyphenPos = tagName.indexOf("-");
    //console.log("hyphenPos => " + hyphenPos);
    if (hyphenPos < 0) {
        // Couldn't find a hypen in the tagName.
        // Dojo widget custom elements cannot be grabbed directly in a
        // drag-and-drop scenario; only their internal elements can be.
        // Thus to check for Dojo custom elements we'll do a further check
        // here to determine whether the parent element's tagName begins
        // with "dojo-". Other custom element libraries might also
        // work like Dojo custom elements. TODO: add checks for those here.
        console.log("findTrueTarget: found no hyphen yet; look at parent element");
        //tagName = node.parentElement.outerHTML.split(" ")[0].slice(1);
        tagName = elementToTagName(node.parentElement);
        console.log("findTrueTarget: tagName => " + tagName);
        hyphenPos = tagName.indexOf("-");
        if (tagName.slice(0, hyphenPos) != "dojo") {
            console.log("findTrueTarget: returning false");
            //return false; // THIS WAS THE RETURN VALUE WHEN THIS FUNCTION WAS A PREDICATE!!!
            return node;
        }
        return node.parentElement;
    }
    console.log("findTrueTarget: returning truthy");
    return node;
}

// The JavaScript "insertBefore" method will need the custom element itself,
// not an inner node target of a JavaScript event like drop and dragstart.
// The "insertBefore" method can be used to insert a dragged element just
// before an element it is dropped upon. See https://mzl.la/3hSaeP1 .
// Dojo widget custom elements can be dragged and dropped like native HTML
// elements, but only their inner elements can be drop or dragstart targets.
// The "findTrueTarget" function is needed to find the actual custom element
// that is to be the "reference node" used to specify the placement of a new
// node into a web page by the "insertBefore" method.
/*
findTrueTarget = function(element) {
    var tagName;
    console.log("findTrueTarget: element => " + element);
    tagName = elementToTagName(element);
    console.log("findTrueTarget: tagName => " + tagName);
    if (isComplexCustomElement(tagName)) {
        return element.parentElement;
    }
    return element;
}
*/

appendHTML = function(location, html) {
    return $(location).append(html);
}

elementTag = function(elem) {
    // Thanks, Aaron Gillion. https://stackoverflow.com/a/34639030
    // FIXME: Check this.
    return elem.innerHTML ? elem.outerHTML.slice(0,elem.outerHTML.indexOf(elem.innerHTML)) : elem.outerHTML;
}

removeCustomElement = function(elem) {
    console.group("elem");
    console.dir(elem.parentNode);
    console.groupEnd();
    elem.parentNode.removeChild(elem);
}
