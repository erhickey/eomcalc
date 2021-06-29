/**
 * contains app specific utility functions
 */

import {addPathSeparator, compareStringsCaseInsensitive} from './util.js';
import {MAX_ACTIVES, MAX_PASSIVES, MAX_SKILLS, ORDER_EQUAL, SKILLS, SKILL_TYPES} from '../constants/constants.js';

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
  const raritySort = compareSkillsByRarity(skill1, skill2);

  if (raritySort !== ORDER_EQUAL) {
    return raritySort;
  }

  return compareStringsCaseInsensitive(skill1.name, skill2.name);
}

/*
 * compareFunction for skills by rarity
 */
export function compareSkillsByRarity(skill1, skill2) {
  return skill2.rarity - skill1.rarity;
}

/*
 * parse url param which may contain list of skills to include in user's build
 */
export function parseBuild(input) {
  if (null == input) {
    return [];
  }

  const build = input.split('.').map(n => SKILLS.find(s => s.id === parseInt(n))).filter(s => null != s);

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
    build.length > MAX_SKILLS
    || build.filter(s => s.type === SKILL_TYPES.ACTIVE).length > MAX_ACTIVES
    || build.filter(s => s.type === SKILL_TYPES.PASSIVE).length > MAX_PASSIVES
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
