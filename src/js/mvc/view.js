/**
 * contains fuctions responsible for rendering the page
 */

import {createBuildLinks} from '../components/build-links.js';
import {createChosenSkills} from '../components/chosen-skills.js';
import {createSkills} from '../components/skill-list.js';
import {createTraits} from '../components/traits.js';
import {SKILL_ID_PREFIX} from '../constants/constants.js';
import {appendChildren, replaceChildren} from '../util/util.js';

const WRAPPER_ID = 'wrapper';
const SKILL_AND_FILTER_WRAPPER_ID = 'skill-and-filter-wrapper';
const SKILL_LIST_ID = 'skill-list';
const BUILD_WRAPPER_ID = 'build-wrapper';
const CHOSEN_SKILLS_ID = 'chosen-skills';
const TRAITS_ID = 'traits';

export function initialRender(skills, chosenSkills) {
  const df = new DocumentFragment();
  df.appendChild(initSkillAndFilterComponents(skills, chosenSkills));
  df.appendChild(initBuildComponents(chosenSkills));
  replaceChildren(document.getElementById(WRAPPER_ID), df);
}

export function buildChanged(chosenSkills, changedSkill, isRemoved) {
  const df = new DocumentFragment();
  df.appendChild(createChosenSkillsComponent(chosenSkills));
  df.appendChild(createTraitsComponent(chosenSkills));
  appendChildren(df, createBuildLinks(chosenSkills));
  replaceChildren(document.getElementById(BUILD_WRAPPER_ID), df);

  const skillEl = document.getElementById(SKILL_ID_PREFIX + changedSkill.id);

  if (isRemoved) {
    skillEl.classList.remove('chosen-skill-list-skill');
  } else {
    skillEl.classList.add('chosen-skill-list-skill');
  }
}

function initSkillAndFilterComponents(skills, chosenSkills) {
  const component = document.createElement('div');
  component.id = SKILL_AND_FILTER_WRAPPER_ID;
  component.appendChild(createSkillListComponent(skills, chosenSkills));
  return component;
}

function createSkillListComponent(skills, chosenSkills) {
  const skillListEl = document.createElement('div');
  skillListEl.id = SKILL_LIST_ID;
  replaceChildren(skillListEl, createSkills(skills, chosenSkills));
  return skillListEl;
}

function initBuildComponents(chosenSkills) {
  const component = document.createElement('div');
  component.id = BUILD_WRAPPER_ID;
  component.appendChild(createChosenSkillsComponent(chosenSkills));
  component.appendChild(createTraitsComponent(chosenSkills));
  appendChildren(component, createBuildLinks(chosenSkills));
  return component;
}

function createChosenSkillsComponent(chosenSkills) {
  if (chosenSkills && chosenSkills.length > 0) {
    const component = document.createElement('div');
    component.id = CHOSEN_SKILLS_ID;
    replaceChildren(component, createChosenSkills(chosenSkills));
    return component;
  }

  const info = document.createElement('div');
  info.classList.add('chosen-skills-info');
  info.innerHTML = 'Choose skills from above to create your build.';
  info.innerHTML += '\nShare your build with the link or discord message below.';
  return info;
}

function createTraitsComponent(chosenSkills) {
  const component = document.createElement('div');
  component.id = TRAITS_ID;
  replaceChildren(component, createTraits(chosenSkills));
  return component;
}
