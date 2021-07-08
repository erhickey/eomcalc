/**
 * reponsible for managing application state
 */

import {MAX_SKILL_LEVEL, MIN_SKILL_LEVEL, NO_CHANGE, SKILL_ADDED, SKILL_REMOVED} from '../constants/app.js';
import {SKILLS} from '../constants/data.js';
import {validBuild} from '../helpers/app.js';

// current chosen skill
let chosenSkills = [];

// skill that details may currently be displayed for
// track this so we know when to show/hide the details
let currentSkillDetail = null;

// the level of the skill to display details for
let skillLevel = MIN_SKILL_LEVEL;

// trait that details may currently be displayed for
// track this so we know when to show/hide the details
let currentTrait = null;

/*
 * called once when application starts
 */
export function initializeState(build) {
  chosenSkills = build;
  return [SKILLS, chosenSkills];
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
 * return true if the skill level changed, otherwise returns false
 */
export function setSkillLevel(level) {
  let lvl = level > MAX_SKILL_LEVEL ? MAX_SKILL_LEVEL : level;
  lvl = level < MIN_SKILL_LEVEL ? MIN_SKILL_LEVEL : level;

  if (skillLevel === lvl) {
    return false;
  }

  skillLevel = lvl;
  return true;
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
  if (currentSkillDetail && currentSkillDetail.skillId === skill.skillId) {
    return false;
  }

  currentSkillDetail = skill;
  return true;
}

/*
 * return the skill that the user last clicked the info button for
 */
export function getCurrentSkillDetail() {
  return currentSkillDetail;
}

/*
 * update the trait to display details for
 * returns true if the trait has changed
 * returns false if the trait didn't change
 */
export function updateTraitDetails(trait) {
  if (currentTrait && currentTrait.id === trait.id) {
    return false;
  }

  currentTrait = trait;
  return true;
}
