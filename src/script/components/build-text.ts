/*
 * build text components include a label, input, and copy to clipboard button
 */

import { Component } from '@components/component';
import { BUILD_TEXTS_CONTAINER_ID, BUILD_URL_ID, DISCORD_MSG_ID } from '@constants/html';
import { IMAGES_DIR } from '@constants/resources';
import { generateBuildDiscordMsg, generateBuildUrlParam } from '@helpers/build-text';
import { Service } from '@mvcs/service';
import { Skill } from '@typez/skill';
import { copyInputText } from '@util/html';

export class BuildTextsComponent extends Component {
  private linkTextInput = BuildTextsComponent.createInput(BUILD_URL_ID);
  private discordTextInput = BuildTextsComponent.createInput(DISCORD_MSG_ID);

  constructor(service: Service) {
    super();
    this.render();
    this.initSubscriptions(service);
  }

  private render(): void {
    this.id = BUILD_TEXTS_CONTAINER_ID;
    this.appendChild(BuildTextsComponent.createBuildText('Link to Build', this.linkTextInput));
    this.appendChild(BuildTextsComponent.createBuildText('Discord Message', this.discordTextInput));
  }

  private initSubscriptions(service: Service): void {
    service.buildChange.subscribe(build => this.update(build));
  }

  private update(build: Skill[]): void {
    this.discordTextInput.value = generateBuildDiscordMsg(build);
    this.linkTextInput.value =
      window.location.origin + window.location.pathname + (build?.length ? '?b=' + generateBuildUrlParam(build) : '');
  }

  private static createBuildText(label: string, input: HTMLInputElement): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('build-text-container');
    el.appendChild(BuildTextsComponent.createLabel(label));
    el.appendChild(BuildTextsComponent.createCopyButton(input));
    el.appendChild(input);
    return el;
  }

  private static createLabel(label: string): HTMLLabelElement {
    const el = document.createElement('label');
    el.innerHTML = label;
    return el;
  }

  private static createCopyButton(input: HTMLInputElement): HTMLInputElement {
    const el = document.createElement('input');
    el.setAttribute('type', 'image');
    el.src = IMAGES_DIR + 'clipboard.webp';
    el.onclick = () => copyInputText(input);
    return el;
  }

  private static createInput(inputId: string): HTMLInputElement {
    const el = document.createElement('input');
    el.id = inputId;
    el.setAttribute('type', 'text');
    el.setAttribute('disabled', 'true');
    return el;
  }
}
