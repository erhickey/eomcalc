/**
 * reponsible for managing application state
 */

import {NO_CHANGE, SKILL_ADDED, SKILL_REMOVED} from '../constants/app.js';
import {SKILLS} from '../constants/data.js';
import {validBuild} from '../helpers/app.js';

const skills = SKILLS;
let chosenSkills = [];

/*
 * called once when application starts
 */
export function initializeState(build) {
  chosenSkills = build;
  return [skills, chosenSkills];
}

/*
 * adds or removes a skill from the build based on the currently chosen skills
 */
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

/*
 * removes a skill from the build
 */
export function removeSkill(skill) {
  chosenSkills = chosenSkills.filter(s => s.id !== skill.id);
  return chosenSkills;
}
