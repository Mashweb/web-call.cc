// As this demo is further developed, the JavaScript inside this script
// will be replaced by BiwaScheme so that eventually its abstract syntax
// tree will be manipulable via an HTML representation of the code.
// The JavaScript here is just to make the inputs lively.

// Vaadin Details Components

customElements.whenDefined('vaadin-details').then(function() {
    const details1 = document.querySelector('#definitions');
    const details2 = document.querySelector('#explanation');
    const bsconsole = document.querySelector('#bsconsole');
    const simple_example = document.querySelector('#simple_example');
    const calculator_example = document.querySelector('#calculator_example');

    console.log("vaadin-details defined");
    if (details1) {
	details1.addEventListener('opened-changed', function(e) {
	    document.getElementById('defstate').textContent = e.detail.value ? "Hide definitions." : "Need some definitions? Click here.";
	});
	details1.opened = null;
    }

    if (details2) {
	details2.addEventListener('opened-changed', function(e) {
	    document.getElementById('exstate').textContent = e.detail.value ? "Hide explanation." : "Show explanation.";
	});
	details2.opened = null;
    }

    if (bsconsole) {
	bsconsole.addEventListener('opened-changed', function(e) {
	    document.getElementById('constate').textContent = e.detail.value ? "Hide Biwascheme console." : "To show the Biwascheme console click here.";
	});
	bsconsole.opened = null;
    }

    if (simple_example) {
	simple_example.addEventListener('opened-changed', function(e) {
	    document.getElementById('simple_example_state').textContent = e.detail.value ? "Hide simple app." : "To try the simple app click here.";
	});
	simple_example.opened = null;
    }

    if (calculator_example) {
	calculator_example.addEventListener('opened-changed', function(e) {
	    document.getElementById('calculator_example_state').textContent = e.detail.value ? "Hide calculator app." : "To try the calculator app click here.";
	});
	calculator_example.opened = null;
    }
});

// Dojo Buttons

var popupButton = document.getElementById('dojoEnabledPopupButton');
var toggleButton = document.getElementById('dojoEnabledToggleButton');
var disabledBasicButton = document.getElementById('dojoDisabledBasicButton');
var disabledIconButton = document.getElementById('dojoDisabledIconButton');
var disabledPopupButton = document.getElementById('dojoDisabledPopupButton');
var disabledToggleButton = document.getElementById('dojoDisabledToggleButton');
var disabledCheckedToggle = document.getElementById('dojoDisabledCheckedToggle');
var disabledUnCheckedToggle = document.getElementById('dojoDisabledUnCheckedToggle');

// Toggle

if (toggleButton) {
    toggleButton.pressed = false;

    toggleButton.addEventListener('click', function () {
	toggleButton.pressed = !toggleButton.pressed;
    });
}
