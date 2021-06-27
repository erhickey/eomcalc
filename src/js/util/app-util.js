/**
 * contains app specific utility functions
 */

import {addPathSeparator} from './util.js';
import {ACTIVE} from '../constants/constants.js';

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

export function stringifyBuild(build) {
  return build.map(s => s.id).join('.');
}
