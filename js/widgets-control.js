// As this demo is further developed, the JavaScript inside this script tag
// will be replaced by BiwaScheme so that eventually its abstract syntax
// tree will be manipulable via an HTML representation of the code.
// The JavaScript here is just to make the inputs lively.

// Vaadin Details Components

customElements.whenDefined('vaadin-details').then(function() {
    const details1 = document.querySelector('#definitions');
    const details2 = document.querySelector('#explanation');

    if (details1) {
	details1.addEventListener('opened-changed', function(e) {
	    document.getElementById('defstate').textContent = e.detail.value ? "Hide definitions" : "Need some definitions? Click here.";
	});
	details1.opened = null;
    }
    
    details2.addEventListener('opened-changed', function(e) {
	document.getElementById('exstate').textContent = e.detail.value ? "Hide explanation" : "Show explanation";
    });
    details2.opened = null;
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
