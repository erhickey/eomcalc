/**
 * entry point, defines eom-calc custom element
 */

import { initialize } from '@mvc/controller';

window.customElements.define(
  'eom-calc',
  class extends HTMLElement {
    connectedCallback(): void {
      initialize(this, new URLSearchParams(window.location.search).get('b') ?? '');
    }
  }
);
