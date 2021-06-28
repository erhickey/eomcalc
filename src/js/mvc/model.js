/**
 * reponsible for managing application state
 */

import {NO_CHANGE, SKILLS, SKILL_ADDED, SKILL_REMOVED, SKILL_TYPES} from '../constants/constants.js';

var skills = SKILLS;
var chosenSkills = [];

export function initializeState(build) {
  chosenSkills = build;
  return [skills, chosenSkills];
}

export function addOrRemoveSkill(skill) {
  if (chosenSkills.some(s => s.id === skill.id)) {
    return [removeSkill(skill), SKILL_REMOVED];
  }

  if (
    chosenSkills.length < 10 &&
    (
      (skill.type === SKILL_TYPES.ACTIVE && chosenSkills.filter(s => s.type === SKILL_TYPES.ACTIVE).length < 5) ||
      (skill.type === SKILL_TYPES.PASSIVE && chosenSkills.filter(s => s.type === SKILL_TYPES.PASSIVE).length < 7)
    )
  ) {
    chosenSkills.push(skill);
    return [chosenSkills, SKILL_ADDED];
  }

  return [chosenSkills, NO_CHANGE];
}

export function removeSkill(skill) {
  chosenSkills = chosenSkills.filter(s => s.id !== skill.id);
  return chosenSkills;
}
