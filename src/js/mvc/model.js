/**
 * reponsible for managing application state
 */

import {MAX_SKILL_LEVEL, MIN_SKILL_LEVEL, NO_CHANGE, SKILL_ADDED, SKILL_REMOVED} from '../constants/app.js';
import {SKILLS} from '../constants/data.js';
import {validBuild} from '../helpers/app.js';

// all the skills
const skills = SKILLS;

// current chosen skill
let chosenSkills = [];

// id of skill that details may currently be displayed for
// track this so we know when to show/hide the details
let currentSkillDetail = -1;

// the level of the skill to display details for
let skillLevel = 0;

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
  if (chosenSkills.some(s => s.skillId === skill.skillId)) {
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
  chosenSkills = chosenSkills.filter(s => s.skillId !== skill.skillId);
  return chosenSkills;
}

/*
 * set the level of the skill to display details for
 */
export function setSkillLevel(level) {
  let lvl = level > MAX_SKILL_LEVEL ? MAX_SKILL_LEVEL : level;
  lvl = level < MIN_SKILL_LEVEL ? MIN_SKILL_LEVEL : level;
  skillLevel = lvl;
}

/*
 * get the level of the skill to display details for
 */
export function getSkillLevel() {
  return skillLevel;
}

/*
 * update the skill to display details for
 * returns true if the skill has changed
 * returns false if the skill didn't change
 */
export function updateSkillDetails(skill) {
  if (currentSkillDetail === skill.skillId) {
    return false;
  }

  currentSkillDetail = skill.skillId;
  return true;
}
