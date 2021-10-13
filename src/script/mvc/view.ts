/**
 * responsible for rendering the page
 * handling all rendering responsibility here helps limit and group DOM updates
 */

import { TRAITS } from '@api/eom';
import { createBuildTexts } from '@components/build-text';
import { createFilters } from '@components/filter';
import { createSkillCards } from '@components/skill-card';
import { createSkillDetail } from '@components/skill-detail';
import { createTraits } from '@components/trait';
import { createTraitDetail } from '@components/trait-detail';
import { CHOSEN_SKILL_CLASS, HIDDEN_CLASS } from '@constants/css';
import * as Html from '@constants/html';
import { compareBuildSkills, compareSkills, compareTraits } from '@helpers/comparators';
import { Filter } from '@typez/filter';
import { Skill } from '@typez/skill';
import { TraitInfo } from '@typez/trait-info';
import { positionElementRelativeTo } from '@util/html';

// number of pixels to offset trait detail from the trait by
const DETAIL_HOVER_OFFSET = 15;

/*
 * called once when the application starts
 *
 * build and add all elements to the given container
 */
export function initialRender(container: HTMLElement, skills: Skill[], build: Skill[]): void {
  const df = new DocumentFragment();
  df.appendChild(initSkillDetailContainer());
  df.appendChild(initTraitDetailContainer());
  df.appendChild(initSkillAndFilterContainers(skills, build));
  df.appendChild(initBuildContainer(build));
  container.replaceChildren(df);
}

function initSkillAndFilterContainers(skills: Skill[], build: Skill[]): HTMLDivElement {
  const el = document.createElement('div');
  el.id = Html.SKILLS_AND_FILTERS_CONTAINER_ID;
  el.appendChild(createFiltersContainer());
  el.appendChild(createSkillListContainer(skills, build));
  return el;
}

function initBuildContainer(build: Skill[]): HTMLDivElement {
  const el = document.createElement('div');
  el.id = Html.BUILD_AND_TRAITS_CONTAINER_ID;
  el.appendChild(createBuildSkillsContainer(build));
  el.appendChild(createTraitsContainer(build));
  el.appendChild(createBuildTexts(build));
  return el;
}

function initSkillDetailContainer(): HTMLDivElement {
  const el = document.createElement('div');
  el.id = Html.SKILL_DETAIL_ID;
  el.classList.add('skill-detail');
  el.classList.add(HIDDEN_CLASS);
  return el;
}

function initTraitDetailContainer(): HTMLDivElement {
  const el = document.createElement('div');
  el.id = Html.TRAIT_DETAIL_ID;
  el.classList.add('trait-detail');
  el.classList.add(HIDDEN_CLASS);
  return el;
}

/*
 * called when the build changes, redraws the components in the build container:
 * build skills or information text, traits, and build texts
 *
 * also adds/removes the chosen skill class to skill cards in the skill list
 */
export function buildChanged(build: Skill[], changedSkill: Skill, isRemoved: boolean): void {
  const df = new DocumentFragment();
  df.appendChild(createBuildSkillsContainer(build));
  df.appendChild(createTraitsContainer(build));
  df.appendChild(createBuildTexts(build));

  const skill = document.getElementById(Html.SKILL_LIST_SKILL_ID_PREFIX + changedSkill.id);

  if (isRemoved) {
    skill?.classList.remove(CHOSEN_SKILL_CLASS);
  } else {
    skill?.classList.add(CHOSEN_SKILL_CLASS);
  }

  document.getElementById(Html.BUILD_AND_TRAITS_CONTAINER_ID)?.replaceChildren(df);
  hideTraitDetail();
}

/*
 * called when the filters change and the skill list needs to be updated
 */
export function filterChanged(skills: Skill[], filters: Filter[], build: Skill[]): void {
  const df = new DocumentFragment();
  df.appendChild(createFiltersContainer(filters));
  df.appendChild(createSkillListContainer(skills, build));
  document.getElementById(Html.SKILLS_AND_FILTERS_CONTAINER_ID)?.replaceChildren(df);
}

function createSkillListContainer(skills: Skill[], build: Skill[]): HTMLDivElement {
  const el = document.createElement('div');
  el.id = Html.SKILL_LIST_CONTAINER_ID;
  el.appendChild(createSkillCards(skills.sort(compareSkills), Html.SKILL_LIST_SKILL_ID_PREFIX, build));
  return el;
}

function createFiltersContainer(filters: Filter[] = []): HTMLDivElement {
  const el = document.createElement('div');
  el.id = Html.FILTERS_CONTAINER_ID;
  el.appendChild(createFilters(filters));
  return el;
}

/*
 * create build skills, or informational text
 */
function createBuildSkillsContainer(build: Skill[]): HTMLDivElement {
  if (build?.length) {
    const el = document.createElement('div');
    el.id = Html.BUILD_SKILLS_CONTAINER_ID;
    el.appendChild(createSkillCards(build.sort(compareBuildSkills), Html.BUILD_SKILL_ID_PREFIX));
    return el;
  }

  const info = document.createElement('div');
  info.classList.add('build-skills-text');
  info.innerHTML = 'Choose skills from above to create your build.';
  info.innerHTML += '\nShare your build with the link or discord message below.';
  return info;
}

function createTraitsContainer(build: Skill[]): HTMLDivElement {
  const el = document.createElement('div');
  el.id = Html.TRAITS_CONTAINER_ID;
  el.appendChild(createTraits(TRAITS.map(t => new TraitInfo(t, build)).sort(compareTraits)));
  return el;
}

export function toggleSkillDetail(): void {
  document.getElementById(Html.SKILL_DETAIL_ID)?.classList.toggle(HIDDEN_CLASS);
}

export function updateSkillDetail(skill: Skill, level: number): void {
  const el = document.getElementById(Html.SKILL_DETAIL_ID);
  el?.replaceChildren(createSkillDetail(skill, level));
  el?.classList.remove(HIDDEN_CLASS);
}

export function hideSkillDetail(): void {
  const el = document.getElementById(Html.SKILL_DETAIL_ID);

  if (!el?.classList.contains(HIDDEN_CLASS)) {
    el?.classList.add(HIDDEN_CLASS);
  }
}

export function toggleTraitDetail(anchorElement: HTMLElement, trait: TraitInfo): void {
  const el = document.getElementById(Html.TRAIT_DETAIL_ID);

  if (el?.classList.contains(HIDDEN_CLASS)) {
    // update trait detail in case the build has changed
    updateTraitDetail(anchorElement, trait);
  } else {
    el?.classList.add(HIDDEN_CLASS);
  }
}

export function updateTraitDetail(anchorElement: HTMLElement, trait: TraitInfo): void {
  const el = document.getElementById(Html.TRAIT_DETAIL_ID);

  if (el) {
    el.replaceChildren(createTraitDetail(trait));
    el.classList.remove(HIDDEN_CLASS);
    positionElementRelativeTo(anchorElement, el, DETAIL_HOVER_OFFSET);
  }
}

export function hideTraitDetail(): void {
  const el = document.getElementById(Html.TRAIT_DETAIL_ID);

  if (!el?.classList.contains(HIDDEN_CLASS)) {
    el?.classList.add(HIDDEN_CLASS);
  }
}
