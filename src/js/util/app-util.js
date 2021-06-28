/**
 * contains app specific utility functions
 */

import {addPathSeparator} from './util.js';
import {ACTIVE, IMAGES_DIR} from '../constants/constants.js';

import {rarities} from '../../data/enums.json';
import {skills} from '../../data/skills.json';

export function createImageNode(imgDir, imgName) {
  const el = document.createElement('img');
  el.setAttribute('loading', 'lazy');
  el.src = addPathSeparator(imgDir) + formatImageName(imgName) + '.webp';
  return el;
}

export function formatImageName(s) {
  return s.toLowerCase().replace(/ /g, '_');
}

export function sortSkills(skill1, skill2) {
  const raritySort = compareSkillRarity(skill1, skill2);

  if (raritySort !== 0) {
    return raritySort;
  }

  if (skill1.name.toUpperCase() < skill2.name.toUpperCase()) {
    return -1;
  }

  return 1;
}

export function compareSkillRarity(skill1, skill2) {
  return rarities[skill2.rarity] - rarities[skill1.rarity];
}

export function parseBuild(input) {
  if (input === null) {
    return [];
  }

  const build = input.split('.').map(n => skills.find(s => s.id === parseInt(n))).filter(s => s !== undefined);

  if (validBuild(build)) {
    return build;
  }

  return [];
}

export function validBuild(build) {
  if (
    build.length > 10 ||
    build.filter(s => s.type === ACTIVE).length > 5
  ) {
    return false;
  }

  return true;
}

export function generateBuildUrlParam(build) {
  return build.map(s => s.id).join('.');
}

export function generateBuildDiscordMsg(build) {
  return build.map(s => ':' + s.name.replace(/ /g, '') + ':').join('');
}

export function createBuildLink(label, value, inputId) {
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
  copyButton.onclick = function() {
    copyInputText(inputId);
  }

  container.appendChild(labelEl);
  container.appendChild(copyButton);
  container.appendChild(inputEl);
  return container;
}

export function copyInputText(inputId) {
  const input = document.getElementById(inputId);
  input.focus();
  input.select();
  input.setSelectionRange(0, 99999); /* For mobile devices */
  document.execCommand("copy");
}
