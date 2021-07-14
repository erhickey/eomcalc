/**
 * responsible for rendering the page
 */

import {createBuildTexts} from '../components/build-text.js';
import {createFilters} from '../components/filter.js';
import {createSkillCards} from '../components/skill-card.js';
import {createSkillDetail} from '../components/skill-detail.js';
import {createTraits} from '../components/trait.js';
import {createTraitDetail} from '../components/trait-detail.js';
import {CHOSEN_SKILL_CLASS, HIDDEN_CLASS} from '../constants/css.js';
import {
  BUILD_SKILL_PREFIX,
  BUILD_CONTAINER_ID,
  CHOSEN_SKILLS_ID,
  FILTERS_ID,
  SKILL_AND_FILTER_CONTAINER_ID,
  SKILL_DETAIL_ID,
  SKILL_ID_PREFIX,
  SKILL_LIST_ID,
  SKILL_LIST_SKILL_PREFIX,
  TRAIT_DETAIL_ID,
  TRAITS_ID
} from '../constants/html.js';
import {compareChosenSkills} from '../helpers/chosen-skills.js';
import {compareSkills} from '../helpers/skills.js';
import {appendChildren, isEmpty, positionElementRelativeTo, replaceChildren} from '../util/util.js';

// number of pixels to offset trait detail from the trait by
const DETAIL_HOVER_OFFSET = 15;

/*
 * called once when the application starts
 *
 * build and add all elements to the given container
 */
export function initialRender(container, skills, chosenSkills) {
  const df = new DocumentFragment();
  df.appendChild(initSkillDetailComponent());
  df.appendChild(initTraitDetailComponent());
  df.appendChild(initSkillAndFilterComponents(skills, chosenSkills));
  df.appendChild(initBuildComponents(chosenSkills));
  appendChildren(container, df);
}

function initSkillAndFilterComponents(skills, chosenSkills) {
  const component = document.createElement('div');
  component.id = SKILL_AND_FILTER_CONTAINER_ID;
  component.appendChild(createFiltersComponent());
  component.appendChild(createSkillList(skills, chosenSkills));
  return component;
}

function initBuildComponents(chosenSkills) {
  const component = document.createElement('div');
  component.id = BUILD_CONTAINER_ID;
  component.appendChild(createChosenSkillsComponent(chosenSkills));
  component.appendChild(createTraitsComponent(chosenSkills));
  appendChildren(component, createBuildTexts(chosenSkills));
  return component;
}

function initSkillDetailComponent() {
  const component = document.createElement('div');
  component.id = SKILL_DETAIL_ID;
  component.classList.add('skill-detail');
  component.classList.add(HIDDEN_CLASS);
  return component;
}

function initTraitDetailComponent() {
  const component = document.createElement('div');
  component.id = TRAIT_DETAIL_ID;
  component.classList.add('trait-detail');
  component.classList.add(HIDDEN_CLASS);
  return component;
}

/*
 * called when the build changes, redraws the components in the build container:
 * chosen skills or information text, traits, and build texts
 *
 * also adds/removes the chosen-skill class to skill cards in the skill list that have been chosen
 */
export function buildChanged(chosenSkills, changedSkill, isRemoved) {
  const df = new DocumentFragment();
  df.appendChild(createChosenSkillsComponent(chosenSkills));
  df.appendChild(createTraitsComponent(chosenSkills));
  appendChildren(df, createBuildTexts(chosenSkills));
  replaceChildren(document.getElementById(BUILD_CONTAINER_ID), df);

  const skillEl = document.getElementById(SKILL_LIST_SKILL_PREFIX + SKILL_ID_PREFIX + changedSkill.skillId);

  if (isRemoved) {
    skillEl.classList.remove(CHOSEN_SKILL_CLASS);
  } else {
    skillEl.classList.add(CHOSEN_SKILL_CLASS);
  }
}

/*
 * called when the filters change and the skill list needs to be updated
 */
export function filterChanged(skills, currentFilters, chosenSkills) {
  const df = new DocumentFragment();
  appendChildren(df, createFiltersComponent(currentFilters));
  appendChildren(df, createSkillList(skills, chosenSkills));
  replaceChildren(document.getElementById(SKILL_AND_FILTER_CONTAINER_ID), df);
}

function createSkillList(skills, chosenSkills) {
  const component = document.createElement('div');
  component.id = SKILL_LIST_ID;
  appendChildren(component, createSkillCards(skills.sort(compareSkills), SKILL_LIST_SKILL_PREFIX, chosenSkills));
  return component;
}

function createFiltersComponent(currentFilters = []) {
  const component = document.createElement('div');
  component.id = FILTERS_ID;
  appendChildren(component, createFilters(currentFilters));
  return component;
}

/*
 * create components of the chosen skills, or informational text
 */
function createChosenSkillsComponent(chosenSkills) {
  if (!isEmpty(chosenSkills)) {
    const component = document.createElement('div');
    component.id = CHOSEN_SKILLS_ID;
    appendChildren(component, createSkillCards(chosenSkills.sort(compareChosenSkills), BUILD_SKILL_PREFIX));
    return component;
  }

  const info = document.createElement('div');
  info.classList.add('chosen-skills-text');
  info.innerHTML = 'Choose skills from above to create your build.';
  info.innerHTML += '\nShare your build with the link or discord message below.';
  return info;
}

function createTraitsComponent(chosenSkills) {
  const component = document.createElement('div');
  component.id = TRAITS_ID;
  appendChildren(component, createTraits(chosenSkills));
  return component;
}

export function toggleSkillDetailComponent() {
  document.getElementById(SKILL_DETAIL_ID).classList.toggle(HIDDEN_CLASS);
}

export function updateSkillDetailComponent(skill, level) {
  const component = document.getElementById(SKILL_DETAIL_ID);
  replaceChildren(component, createSkillDetail(skill, level));
  component.classList.remove(HIDDEN_CLASS);
}

export function hideSkillDetailComponent() {
  document.getElementById(SKILL_DETAIL_ID).classList.add(HIDDEN_CLASS);
}

export function toggleTraitDetailComponent() {
  document.getElementById(TRAIT_DETAIL_ID).classList.toggle(HIDDEN_CLASS);
}

export function updateTraitDetailComponent(anchorElement, trait) {
  const component = document.getElementById(TRAIT_DETAIL_ID);
  replaceChildren(component, createTraitDetail(trait));
  component.classList.remove(HIDDEN_CLASS);
  positionElementRelativeTo(anchorElement, component, DETAIL_HOVER_OFFSET);
}

export function hideTraitDetailComponent() {
  document.getElementById(TRAIT_DETAIL_ID).classList.add(HIDDEN_CLASS);
}
