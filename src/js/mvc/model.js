/**
 * reponsible for managing application state
 */

import {MAX_SKILL_LEVEL, MIN_SKILL_LEVEL, NO_CHANGE, SKILL_ADDED, SKILL_REMOVED} from '../constants/app.js';
import {SKILLS} from '../constants/data.js';
import {validBuild} from '../helpers/build-validator.js';

// user's current build
let chosenSkills = [];

// skill that skill detail component may be displaying
let currentSkillDetail = null;

// level of the skill that skill detail may be displaying
let skillLevel = MIN_SKILL_LEVEL;

// trait that trait detail component may be displaying
let currentTrait = null;

// filters that are currently active
let currentFilters = [];

/*
 * called once when application starts
 *
 * returns the full list of skills, and the user's current build
 */
export function initializeState(build = []) {
  chosenSkills = build;
  return [SKILLS, chosenSkills];
}

export function getChosenSkills() {
  return chosenSkills;
}

/*
 * add or remove a skill from the build
 *
 * if the skill is already in the build it will be removed, otherwise it will be added
 * no change to the build will be made if the resulting build is invalid
 *
 * returns the updated build and a value indicating what change took place
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
 * remove skill from the build
 *
 * returns the updated build
 */
function removeSkill(skill) {
  chosenSkills = chosenSkills.filter(s => s.skillId !== skill.skillId);
  return chosenSkills;
}

/*
 * set the level of the skill to display in skill detail
 *
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
 * get the level of the skill to display in skill detail
 */
export function getSkillLevel() {
  return skillLevel;
}

/*
 * update the skill to display in skill detail
 *
 * returns true if the skill has changed
 * returns false if the skill didn't change
 */
export function updateSkillDetail(skill) {
  if (currentSkillDetail && currentSkillDetail.skillId === skill.skillId) {
    return false;
  }

  currentSkillDetail = skill;
  return true;
}

/*
 * return the skill that should be currently displayed in skill detail
 */
export function getCurrentSkillDetail() {
  return currentSkillDetail;
}

/*
 * update the trait to display in trait detail
 *
 * returns true if the trait has changed
 * returns false if the trait didn't change
 */
export function updateTraitDetail(trait) {
  if (currentTrait && currentTrait.id === trait.id) {
    return false;
  }

  currentTrait = trait;
  return true;
}

/*
 * if the filter is already active, remove it, otherwise add it to the list of active filters
 *
 * returns the resulting list of active filters
 */
export function addOrRemoveFilter(filter) {
  if (currentFilters.some(cf => cf.key === filter.key && cf.value === filter.value)) {
    currentFilters = currentFilters.filter(cf => !(cf.key === filter.key && cf.value === filter.value));
  } else {
    currentFilters.push(filter);
  }

  return currentFilters;
}

/*
 * clear currentFilters
 */
export function clearFilters() {
  currentFilters = [];
}
