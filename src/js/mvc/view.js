/**
 * contains fuctions responsible for rendering the page
 */

import {createBuildLinks} from '../components/build-links.js';
import {createChosenSkills} from '../components/chosen-skills.js';
import {createSkills} from '../components/skill-list.js';
import {createTraits} from '../components/traits.js';
import {
  BUILD_WRAPPER_ID,
  CHOSEN_SKILLS_ID,
  SKILL_AND_FILTER_WRAPPER_ID,
  SKILL_ID_PREFIX,
  SKILL_LIST_ID,
  TRAITS_ID,
  WRAPPER_ID
} from '../constants/html.js';
import {appendChildren, isEmpty, replaceChildren} from '../util/util.js';

/*
 * called once when the application starts
 * build and add all elements to the DOM
 */
export function initialRender(skills, chosenSkills) {
  const df = new DocumentFragment();
  df.appendChild(initSkillAndFilterComponents(skills, chosenSkills));
  df.appendChild(initBuildComponents(chosenSkills));
  appendChildren(document.getElementById(WRAPPER_ID), df);
}

/*
 * called once when application starts to initialize the skill list and filter
 */
function initSkillAndFilterComponents(skills, chosenSkills) {
  const component = document.createElement('div');
  component.id = SKILL_AND_FILTER_WRAPPER_ID;
  component.appendChild(createSkillListComponent(skills, chosenSkills));
  return component;
}

/*
 * called once when application starts to initialize all build components
 * chosen skill list or informational text, traits and details, and build links
 */
function initBuildComponents(chosenSkills) {
  const component = document.createElement('div');
  component.id = BUILD_WRAPPER_ID;
  component.appendChild(createChosenSkillsComponent(chosenSkills));
  component.appendChild(createTraitsComponent(chosenSkills));
  appendChildren(component, createBuildLinks(chosenSkills));
  return component;
}

/*
 * called when the build changes, redraws the components of the build
 * chosen skill list or informational text, traits and details, and build links
 */
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

/*
 * create components of the skill list
 */
function createSkillListComponent(skills, chosenSkills) {
  const skillListEl = document.createElement('div');
  skillListEl.id = SKILL_LIST_ID;
  appendChildren(skillListEl, createSkills(skills, chosenSkills));
  return skillListEl;
}

/*
 * create components of the chosen skills, or informational text
 */
function createChosenSkillsComponent(chosenSkills) {
  if (!isEmpty(chosenSkills)) {
    const component = document.createElement('div');
    component.id = CHOSEN_SKILLS_ID;
    appendChildren(component, createChosenSkills(chosenSkills));
    return component;
  }

  const info = document.createElement('div');
  info.classList.add('chosen-skills-info');
  info.innerHTML = 'Choose skills from above to create your build.';
  info.innerHTML += '\nShare your build with the link or discord message below.';
  return info;
}

/*
 * create trait components reflecting data about the build
 */
function createTraitsComponent(chosenSkills) {
  const component = document.createElement('div');
  component.id = TRAITS_ID;
  appendChildren(component, createTraits(chosenSkills));
  return component;
}
