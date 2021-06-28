/**
 * contains app specific utility functions
 */

import {addPathSeparator} from './util.js';
import {SKILLS, SKILL_TYPES} from '../constants/constants.js';

/*
 * create standard image node
 */
export function createImageNode(imgDir, imgName) {
  const el = document.createElement('img');
  el.setAttribute('loading', 'lazy');
  el.src = addPathSeparator(imgDir) + formatImageName(imgName) + '.webp';
  return el;
}

/*
 * format image name to match app standard
 */
export function formatImageName(s) {
  return s.toLowerCase().replace(/ /g, '_');
}

/*
 * compareFunction for skills
 */
export function compareSkills(skill1, skill2) {
  const raritySort = compareSkillRarity(skill1, skill2);

  if (raritySort !== 0) {
    return raritySort;
  }

  if (skill1.name.toUpperCase() < skill2.name.toUpperCase()) {
    return -1;
  }

  return 1;
}

/*
 * compareFunction for skills by rarity
 */
export function compareSkillRarity(skill1, skill2) {
  return skill2.rarity - skill1.rarity;
}

/*
 * parse url param which may contain list of skills to include in user's build
 */
export function parseBuild(input) {
  if (input === null) {
    return [];
  }

  const build = input.split('.').map(n => SKILLS.find(s => s.id === parseInt(n))).filter(s => s !== undefined);

  if (validBuild(build)) {
    return build;
  }

  return [];
}

/*
 * validate build conforms to game rules
 */
export function validBuild(build) {
  if (
    build.length > 10 ||
    build.filter(s => s.type === SKILL_TYPES.ACTIVE).length > 5 ||
    build.filter(s => s.type === SKILL_TYPES.PASSIVE).length > 7
  ) {
    return false;
  }

  return true;
}

/*
 * generate build url param from list of skills
 */
export function generateBuildUrlParam(build) {
  return build.map(s => s.id).join('.');
}

/*
 * generate discord message from list of skills
 */
export function generateBuildDiscordMsg(build) {
  return build.map(s => ':' + s.name.replace(/ /g, '') + ':').join('');
}
