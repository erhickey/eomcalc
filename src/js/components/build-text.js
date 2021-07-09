/*
 * build text components include a label, input, and copy to clipboard button
 */

import {BUILD_URL_ID, DISCORD_MSG_ID} from '../constants/html.js';
import {IMAGES_DIR} from '../constants/resources.js';
import {generateBuildDiscordMsg, generateBuildUrlParam} from '../helpers/build-text.js';
import {copyInputText, isEmpty} from '../util/util.js';

/*
 * creates discord message and build url components
 */
export function createBuildTexts(skills) {
  const buildUrl
    = window.location.origin
    + window.location.pathname
    + (isEmpty(skills) ? '' : '?b=' + generateBuildUrlParam(skills));

  return [
    createBuildText('Link to Build', buildUrl, BUILD_URL_ID),
    createBuildText('Discord Message', generateBuildDiscordMsg(skills), DISCORD_MSG_ID)
  ];
}

/*
 * create a single build text component
 */
function createBuildText(label, value, inputId) {
  const component = document.createElement('div');
  component.classList.add('build-text-container');
  component.appendChild(createBuildTextLabel(label));
  component.appendChild(createBuildTextCopyButton(inputId));
  component.appendChild(createBuildTextInput(value, inputId));
  return component;
}

function createBuildTextInput(value, inputId) {
  const component = document.createElement('input');
  component.setAttribute('type', 'text');
  component.setAttribute('disabled', 'true');
  component.value = value;
  component.id = inputId;
  return component;
}

function createBuildTextLabel(label) {
  const component = document.createElement('label');
  component.innerHTML = label;
  return component;
}

function createBuildTextCopyButton(inputId) {
  const component = document.createElement('input');
  component.setAttribute('type', 'image');
  component.src = IMAGES_DIR + 'clipboard.webp';
  component.onclick = () => copyInputText(inputId);
  return component;
}
