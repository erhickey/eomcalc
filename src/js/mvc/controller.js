/**
 * responsible for responding to user input
 *
 * updates the model as necessary, and calls appropriate functions in view
 */

import {
  addOrRemoveSkill,
  getCurrentSkillDetail,
  getSkillLevel,
  initializeState,
  setSkillLevel,
  updateSkillDetail,
  updateTraitDetail
} from './model.js';
import {
  buildChanged,
  hideSkillDetailComponent,
  hideTraitDetailComponent,
  initialRender,
  toggleSkillDetailComponent,
  toggleTraitDetailComponent,
  updateSkillDetailComponent,
  updateTraitDetailComponent
} from './view.js';
import {NO_CHANGE, SKILL_REMOVED} from '../constants/app.js';
import {ESCAPE_KEY} from '../constants/constants.js';

/*
 * called once when the app starts
 *
 * first argument is the html element the calculator will be inserted into
 * second argument may be a build that the user provided
 */
export function initializeApp(container, build) {
  const [skills, chosenSkills] = initializeState(build);
  initialRender(container, skills, chosenSkills);

  // hide skill detail when esc is pressed
  document.onkeydown = evt => {
    const evnt = evt || window.event;

    if (ESCAPE_KEY === evnt.keyCode) {
      hideSkillDetailComponent();
      hideTraitDetailComponent();
    }
  };
}

/*
 * called when a skill card is clicked
 */
export function onSkillClick(skill) {
  const [chosenSkills, change] = addOrRemoveSkill(skill);

  if (change === NO_CHANGE) {
    return;
  }

  buildChanged(chosenSkills, skill, change === SKILL_REMOVED);
}

/*
 * called when skill card detail button is clicked
 */
export function onSkillDetailClick(skill, level = null) {
  if (level) {
    setSkillLevel(level);
  }

  if (updateSkillDetail(skill)) {
    updateSkillDetailComponent(skill, getSkillLevel());
  } else {
    toggleSkillDetailComponent();
  }
}

/*
 * called when the skill detail close button is clicked
 */
export function hideSkillDetail() {
  hideSkillDetailComponent();
}

/*
 * called when a skill detail level is clicked
 */
export function onSkillLevelClick(level) {
  if (setSkillLevel(level)) {
    updateSkillDetailComponent(getCurrentSkillDetail(), getSkillLevel());
  }
}

/*
 * called when a trait is clicked
 */
export function onTraitClick(anchorElement, trait) {
  if (updateTraitDetail(trait)) {
    updateTraitDetailComponent(anchorElement, trait);
  } else {
    toggleTraitDetailComponent();
  }
}

/*
 * called when the trait detail close button is clicked
 */
export function hideTraitDetail() {
  hideTraitDetailComponent();
}
