/*
 * entry point, defines eom-calc custom element
 */

import { Controller } from '@mvcs/controller';
import { Service } from '@mvcs/service';
import { initialize } from '@mvcs/view';

window.customElements.define(
  'eom-calc',
  class extends HTMLElement {
    connectedCallback(): void {
      const service = new Service(new URLSearchParams(window.location.search).get('b') ?? '');
      initialize(new Controller(service), service, this);
    }
  }
);
