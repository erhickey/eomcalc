import {ACTIVE} from './constants.js';

import {rarities} from '../data/enums.json';
import {skills} from '../data/skills.json';

export function format(str, args) {
  var i = 0;

  return str.replace(/{}/g, function () {
    return typeof args[i] != 'undefined' ? args[i++] : '';
  });
};

export function replaceChildren(container, children) {
  container.replaceChildren();
  const df = new DocumentFragment();

  for (let c of children) {
    df.appendChild(c);
  }

  container.appendChild(df);
}

export function createImageNode(imgDir, imgName) {
  const el = document.createElement('img');
  el.setAttribute('loading', 'lazy');
  el.src = addTrailingSlash(imgDir) + formatImageName(imgName) + '.webp';
  return el;
}

export function addTrailingSlash(dir) {
  return dir.endsWith('/') ? dir : dir + '/';
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
    return null;
  }

  const build = input.split('.').map(n => skills.find(s => s.id === parseInt(n))).filter(s => s !== undefined);

  if (validBuild(build)) {
    return build;
  }

  return null;
}

export function validBuild(build) {
  if (build.length > 10) {
    return false;
  }

  if (build.filter(s => s.type === ACTIVE).length > 5) {
    return false;
  }

  return true;
}

export function stringifyBuild(build) {
  return build.map(s => s.id).join('.');
}
