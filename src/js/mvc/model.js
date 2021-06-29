/**
 * reponsible for managing application state
 */

import {NO_CHANGE, SKILLS, SKILL_ADDED, SKILL_REMOVED} from '../constants/constants.js';
import {validBuild} from '../util/app-util.js';

const skills = SKILLS;
let chosenSkills = [];

export function initializeState(build) {
  chosenSkills = build;
  return [skills, chosenSkills];
}

export function addOrRemoveSkill(skill) {
  if (chosenSkills.some(s => s.id === skill.id)) {
    return [removeSkill(skill), SKILL_REMOVED];
  }

  chosenSkills.push(skill);

  if (validBuild(chosenSkills)) {
    return [chosenSkills, SKILL_ADDED];
  }

  chosenSkills.pop();
  return [chosenSkills, NO_CHANGE];
}

export function removeSkill(skill) {
  chosenSkills = chosenSkills.filter(s => s.id !== skill.id);
  return chosenSkills;
}
