/**
 * responsible for responding to user input
 */

import {
  addOrRemoveSkill,
  getSkillLevel,
  initializeState,
  removeSkill,
  setSkillLevel,
  updateSkillDetails
} from './model.js';
import {
  buildChanged,
  hideSkillDetailsComponent,
  initialRender,
  showSkillDetailsComponent,
  toggleSkillDetailsComponent
} from './view.js';
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

/*
 * called when skill info button is clicked on a skill list skill or chosen skill
 */
export function onSkillDetailsClick(skill, level = null) {
  if (level) {
    setSkillLevel(level);
  }

  if (updateSkillDetails(skill)) {
    showSkillDetailsComponent(skill, getSkillLevel());
  } else {
    toggleSkillDetailsComponent();
  }
}

/*
 * hide the skill details component
 */
export function hideSkillDetails() {
  hideSkillDetailsComponent();
}
