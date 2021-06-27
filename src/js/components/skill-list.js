/*
 * contains functions which create the elements that make up the list of skills to choose from
 */

import {SKILL_ID_PREFIX, SKILL_IMAGES_DIR, TRAIT_IMAGES_DIR} from '../constants/constants.js';
import {onSkillClick} from '../mvc/controller.js';
import {createImageNode, sortSkills} from '../util/app-util.js';

export function createSkills(skills, chosenSkills) {
  return skills.sort(sortSkills).map(s => createSkillListComponent(s, chosenSkills));
}

function createSkillListComponent(skill, chosenSkills) {
  const component = document.createElement('div');
  component.id = SKILL_ID_PREFIX + skill.id;
  component.classList.add('skill-list-skill', skill.rarity.toLowerCase());

  if (chosenSkills && chosenSkills.some(s => s.id === skill.id)) {
    component.classList.add('chosen-skill-list-skill');
  }

  const skillImage = createImageNode(SKILL_IMAGES_DIR, skill.name);

  const footer = document.createElement('div');
  footer.classList.add('skill-list-skill-footer');

  const primaryTrait = createImageNode(TRAIT_IMAGES_DIR, skill.primaryTrait);
  primaryTrait.classList.add('skill-list-skill-primary-trait');

  const secondaryTrait = createImageNode(TRAIT_IMAGES_DIR, skill.secondaryTrait);
  secondaryTrait.classList.add('skill-list-skill-secondary-trait');

  footer.appendChild(primaryTrait);
  footer.appendChild(secondaryTrait);
  component.appendChild(skillImage);
  component.appendChild(footer);

  component.onclick = function () {
    onSkillClick(skill);
  }

  return component;
}
