/**
 * application entry point
 */

import {initializeApp} from './mvc/controller.js';
import {parseBuild} from './helpers/build-strings.js';

window.customElements.define('eom-calc', class extends HTMLElement {
  connectedCallback() {
    initializeApp(this, parseBuild(new URLSearchParams(window.location.search).get('b')));
  }
});
