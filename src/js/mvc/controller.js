/**
 * responsible for responding to user input
 */

import {
  addOrRemoveSkill,
  getCurrentSkillDetail,
  getSkillLevel,
  initializeState,
  removeSkill,
  setSkillLevel,
  updateSkillDetails,
  updateTraitDetails
} from './model.js';
import {
  buildChanged,
  hideSkillDetailsComponent,
  hideTraitDetailsComponent,
  initialRender,
  toggleSkillDetailsComponent,
  toggleTraitDetailsComponent,
  updateSkillDetailsComponent,
  updateTraitDetailsComponent
} from './view.js';
import {NO_CHANGE, SKILL_REMOVED} from '../constants/app.js';
import {ESCAPE_KEY} from '../constants/constants.js';

/*
 * called once when app starts
 * first argument is the html element the calculator will be inserted into
 * second argument is a build that from the build param
 */
export function initializeApp(container, build) {
  const [skills, chosenSkills] = initializeState(build);
  initialRender(container, skills, chosenSkills);

  // hide skill details when esc is pressed
  document.onkeydown = evt => {
    const evnt = evt || window.event;

    if (ESCAPE_KEY === evnt.keyCode) {
      hideSkillDetailsComponent();
      hideTraitDetailsComponent();
    }
  };
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
    updateSkillDetailsComponent(skill, getSkillLevel());
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

/*
 * set skill detail level
 */
export function onSkillLevelClick(level) {
  if (setSkillLevel(level)) {
    updateSkillDetailsComponent(getCurrentSkillDetail(), getSkillLevel());
  }
}

/*
 * called when user clicks on a trait to display the trait details
 */
export function onTraitClick(anchorElement, trait) {
  if (updateTraitDetails(trait)) {
    updateTraitDetailsComponent(anchorElement, trait);
  } else {
    toggleTraitDetailsComponent();
  }
}

/*
 * hide the trait details component
 */
export function hideTraitDetails() {
  hideTraitDetailsComponent();
}
