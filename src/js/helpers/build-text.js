/**
 * helper functions for creating/parsing build URL and discord strings
 */

import {validBuild} from './app.js';
import {SKILLS} from '../constants/data.js';

/*
 * parse url param which may contain list of skills to include in user's build
 */
export function parseBuild(input) {
  if (null == input) {
    return [];
  }

  const build = input.split('.').map(n => SKILLS.find(s => s.skillId === parseInt(n))).filter(s => null != s);

  if (validBuild(build)) {
    return build;
  }

  return [];
}

/*
 * generate build url param from list of skills
 */
export function generateBuildUrlParam(skills) {
  return skills.map(s => s.skillId).join('.');
}

/*
 * generate discord message from list of skills
 */
export function generateBuildDiscordMsg(skills) {
  return skills.map(s => ':' + s.skillName.replace(/ /g, '') + ':').join('');
}
