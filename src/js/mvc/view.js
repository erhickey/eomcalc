/**
 * contains fuctions responsible for rendering the page
 */

import {createChosenSkills} from '../components/chosen-skills.js';
import {createSkills} from '../components/skill-list.js';
import {SKILL_ID_PREFIX} from '../constants/constants.js';
import {replaceChildren} from '../util/util.js';

const WRAPPER_ID = 'wrapper';
const SKILL_LIST_ID = 'skill-list';
const CHOSEN_SKILLS_ID = 'chosen-skills';

export function initialRender(skills, chosenSkills) {
  const df = new DocumentFragment();
  df.appendChild(createSkillListComponent(skills, chosenSkills));
  df.appendChild(createChosenSkillsComponent(chosenSkills));
  replaceChildren(document.getElementById(WRAPPER_ID), df);
}

export function buildChanged(chosenSkills, changedSkill, isRemoved) {
  replaceChildren(document.getElementById(CHOSEN_SKILLS_ID), createChosenSkillsComponent(chosenSkills));
  const skillEl = document.getElementById(SKILL_ID_PREFIX + changedSkill.id);

  if (isRemoved) {
    skillEl.classList.remove('chosen-skill-list-skill');
  } else {
    skillEl.classList.add('chosen-skill-list-skill');
  }
}

function createSkillListComponent(skills, chosenSkills) {
  const skillListEl = document.createElement('div');
  skillListEl.id = SKILL_LIST_ID;
  replaceChildren(skillListEl, createSkills(skills, chosenSkills));
  return skillListEl;
}

function createChosenSkillsComponent(chosenSkills) {
  const chosenSkillsEl = document.createElement('div');
  chosenSkillsEl.id = CHOSEN_SKILLS_ID;
  replaceChildren(chosenSkillsEl, createChosenSkills(chosenSkills));
  return chosenSkillsEl;
}
