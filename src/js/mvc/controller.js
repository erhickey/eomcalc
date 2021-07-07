/**
 * responsible for responding to user input
 */

import {addOrRemoveSkill, initializeState, removeSkill} from './model.js';
import {buildChanged, initialRender} from './view.js';
import {NO_CHANGE, SKILL_REMOVED} from '../constants/app.js';

/*
 * called once when app starts
 * first argument is the html element the calculator will be inserted into
 * second argument is a build that from the build param
 */
export function initializeApp(container, build) {
  const [skills, chosenSkills] = initializeState(build);
  initialRender(container, skills, chosenSkills);
}

/*
 * called when user clicks a skill in the skill list
 */
export function onSkillClick(skill) {
  const [chosenSkills, change] = addOrRemoveSkill(skill);

  if (change === NO_CHANGE) {
    return;
  }

  buildChanged(chosenSkills, skill, change === SKILL_REMOVED);
}

/*
 * called when user clicks a skill in the chosen skills list
 */
export function onChosenSkillClick(skill) {
  buildChanged(removeSkill(skill), skill, true);
}
