/*
 * contains functions which create the elements to display build links
 */

import {IMAGES_DIR} from '../constants/constants.js';
import {generateBuildDiscordMsg, generateBuildUrlParam} from '../util/app-util.js';
import {copyInputText, isEmpty} from '../util/util.js';

export function createBuildLinks(skills) {
  const buildParam = isEmpty(skills) ? '' : '?b=' + generateBuildUrlParam(skills);
  const buildLinkEl = createBuildLink(
    'Link to Build',
    window.location.origin + window.location.pathname + buildParam,
    'build-link-input'
  );

  const discordMsgEl = createBuildLink('Discord Message', generateBuildDiscordMsg(skills), 'discord-msg-input');

  return [buildLinkEl, discordMsgEl];
}

function createBuildLink(label, value, inputId) {
  const container = document.createElement('div');
  container.classList.add('build-link-container');

  const inputEl = document.createElement('input');
  inputEl.setAttribute('type', 'text');
  inputEl.setAttribute('disabled', 'true');
  inputEl.value = value;
  inputEl.id = inputId;

  const labelEl = document.createElement('label');
  labelEl.innerHTML = label;

  const copyButton = document.createElement('input');
  copyButton.setAttribute('type', 'image');
  copyButton.src = IMAGES_DIR + 'clipboard.webp';
  copyButton.onclick = function() { copyInputText(inputId); };

  container.appendChild(labelEl);
  container.appendChild(copyButton);
  container.appendChild(inputEl);
  return container;
}
