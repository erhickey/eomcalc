/**
 * reponsible for managing application state
 */

import {ACTIVE, NO_CHANGE, PASSIVE, SKILL_ADDED, SKILL_REMOVED} from '../constants/constants.js';

import {skills as SKILLS} from '../../data/skills.json';

var skills = SKILLS;
var chosenSkills = [];

export function initializeState(build) {
  chosenSkills = build;
  return [skills, chosenSkills];
}

export function addOrRemoveSkill(skill) {
  if (chosenSkills.find(s => s.id === skill.id)) {
    return [removeSkill(skill), SKILL_REMOVED];
  }

  if (
    chosenSkills.length < 10 &&
    (
      (skill.type === ACTIVE && chosenSkills.filter(s => s.type === ACTIVE).length < 5) ||
      (skill.type === PASSIVE && chosenSkills.filter(s => s.type === PASSIVE).length < 7)
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
