import {chooseSkill} from './chosen-skills.js';
import {SKILL_IMAGES_DIR, TRAIT_IMAGES_DIR} from './constants.js';
import {createImageNode, sortSkills} from './util.js';

export function createSkillListComponents(skills) {
  return skills.sort(sortSkills).map(s => createSkillListComponent(s));
}

function createSkillListComponent(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-list-skill', skill.rarity.toLowerCase());

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
    chooseSkill(skill);
  }

  return component;
}
