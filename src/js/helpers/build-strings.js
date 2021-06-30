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

  const build = input.split('.').map(n => SKILLS.find(s => s.id === parseInt(n))).filter(s => null != s);

  if (validBuild(build)) {
    return build;
  }

  return [];
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
