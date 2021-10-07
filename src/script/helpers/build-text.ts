/**
 * helper functions for creating/parsing build URL and discord strings
 */

import { validBuild } from '@helpers/build-validator';
import { Skill } from '@typez/skill';

/*
 * parse url param which may contain list of skills to include in user's build
 */
export function parseBuild(input: string, skills: Skill[]): Skill[] {
  // parse valid numbers from period separated string
  const skillIds = input
    .split('.')
    .map(s => {
      try {
        return parseInt(s);
      } catch (_error) {
        return NaN;
      }
    })
    .filter(n => !isNaN(n));

  const build: Skill[] = [];

  // get skills with ids matching the parsed numbers
  skillIds.forEach(n => {
    const skill = skills.find(s => s.id === n);
    if (skill) {
      build.push(skill);
    }
  });

  return validBuild(build) ? build : [];
}

/*
 * generate build url param from list of skills
 */
export function generateBuildUrlParam(skills: Skill[]): string {
  return skills.map(s => s.id).join('.');
}

/*
 * generate discord message from list of skills
 */
export function generateBuildDiscordMsg(skills: Skill[]): string {
  return skills.map(s => ':' + s.name.replace(/ /g, '') + ':').join('');
}
