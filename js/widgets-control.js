// As this demo is further developed, the JavaScript inside this script tag
// will be replaced by BiwaScheme so that eventually its abstract syntax
// tree will be manipulable via an HTML representation of the code.
// The JavaScript here is just to make the inputs lively.

// Vaadin Details Components

customElements.whenDefined('vaadin-details').then(function() {
    const details1 = document.querySelector('#definitions');
    const details2 = document.querySelector('#explanation');

    details1.addEventListener('opened-changed', function(e) {
	document.getElementById('defstate').textContent = e.detail.value ? "Hide definitions" : "Need some definitions? Click here.";
    });
    details1.opened = null;
    
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

toggleButton.pressed = true;

toggleButton.addEventListener('click', function () {
    toggleButton.pressed = !toggleButton.pressed;
});
popupButton.popup = {
    expanded: false,
    id: 'dojoPopupButton'
};

disabledBasicButton.disabled = true;
disabledIconButton.disabled = true;
disabledPopupButton.popup = {
    expanded: false,
    id: 'dojoPopupButton'
};
disabledPopupButton.disabled = true;
disabledToggleButton.disabled = true;
disabledToggleButton.addEventListener('click', function () {
    disabledToggleButton.pressed = !disabledToggleButton.pressed;
});

// Checkbox
var checkbox = document.getElementById('dojoCheckedCheckbox');
var disabledCheckedCheckbox = document.getElementById('dojoDisabledCheckedCheckbox');
var disabledCheckbox = document.getElementById('dojoDisabledUnCheckedCheckbox');

checkbox.checked = true;
checkbox.addEventListener('change', function () {
    checkbox.checked = !checkbox.checked;
});

disabledCheckedCheckbox.checked = true;
disabledCheckedCheckbox.disabled = true;
disabledCheckbox.disabled = true;

// Toggle
var toggle = document.getElementById('dojoCheckedToggle');
var disabledCheckedToggle = document.getElementById('dojoDisabledCheckedToggle');
var disabledUnCheckedToggle = document.getElementById('dojoDisabledUnCheckedToggle');

toggle.checked = true;
toggle.addEventListener('change', function () {
    toggle.checked = !toggle.checked;
});

disabledCheckedToggle.disabled = true;
disabledCheckedToggle.checked = true;
disabledUnCheckedToggle.disabled = true;
disabledUnCheckedToggle.checked = false;

// Radio
var radioOne = document.getElementById('dojoRadioOne');
var radioTwo = document.getElementById('dojoRadioTwo');
var disabledRadio = document.getElementById('dojoDisabledUnCheckedRadio');

radioOne.checked = true;
