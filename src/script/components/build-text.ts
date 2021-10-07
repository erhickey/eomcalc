/*
 * build text components include a label, input, and copy to clipboard button
 */

import { BUILD_URL_ID, DISCORD_MSG_ID } from '@constants/html';
import { IMAGES_DIR } from '@constants/resources';
import { generateBuildDiscordMsg, generateBuildUrlParam } from '@helpers/build-text';
import { Skill } from '@typez/skill';
import { copyInputText } from '@util/util';

/*
 * creates discord message and build url components
 */
export function createBuildTexts(skills: Skill[]): DocumentFragment {
  const buildUrl =
    window.location.origin + window.location.pathname + (skills?.length ? '?b=' + generateBuildUrlParam(skills) : '');

  const df = new DocumentFragment();
  df.appendChild(createBuildTextComponent('Link to Build', buildUrl, BUILD_URL_ID));
  df.appendChild(createBuildTextComponent('Discord Message', generateBuildDiscordMsg(skills), DISCORD_MSG_ID));
  return df;
}

function createBuildTextComponent(label: string, value: string, inputId: string): HTMLDivElement {
  const buildInput = createBuildTextInput(value, inputId);

  const el = document.createElement('div');
  el.classList.add('build-text-container');
  el.appendChild(createBuildTextLabel(label));
  el.appendChild(createBuildTextCopyButton(buildInput));
  el.appendChild(buildInput);
  return el;
}

function createBuildTextInput(value: string, inputId: string): HTMLInputElement {
  const el = document.createElement('input');
  el.setAttribute('type', 'text');
  el.setAttribute('disabled', 'true');
  el.value = value;
  el.id = inputId;
  return el;
}

function createBuildTextLabel(label: string): HTMLLabelElement {
  const el = document.createElement('label');
  el.innerHTML = label;
  return el;
}

function createBuildTextCopyButton(input: HTMLInputElement): HTMLInputElement {
  const el = document.createElement('input');
  el.setAttribute('type', 'image');
  el.src = IMAGES_DIR + 'clipboard.webp';
  el.onclick = () => copyInputText(input);
  return el;
}
