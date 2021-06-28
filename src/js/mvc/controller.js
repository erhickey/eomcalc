/**
 * responsible for responding to user input
 */

import {addOrRemoveSkill, initializeState, removeSkill} from './model.js';
import {buildChanged, initialRender} from './view.js';
import {NO_CHANGE, SKILL_REMOVED} from '../constants/constants.js';

export function initializeApp(build) {
  const [skills, chosenSkills] = initializeState(build);
  initialRender(skills, chosenSkills);
}

export function onSkillClick(skill) {
  const [chosenSkills, change] = addOrRemoveSkill(skill);

  if (change === NO_CHANGE) {
    return;
  }

  buildChanged(chosenSkills, skill, change === SKILL_REMOVED);
}

export function onChosenSkillClick(skill) {
  buildChanged(removeSkill(skill), skill, true);
}
