/*
 * contains functions which create the elements that make up the list of skills to choose from
 */

import {createSkillInfoComponent} from './skill-info.js';
import {RARITY_MAP, TRAIT_MAP} from '../constants/data.js';
import {SKILL_ID_PREFIX} from '../constants/html.js';
import {SKILL_IMAGES_DIR, TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {createImageNode} from '../helpers/components.js';
import {compareSkills} from '../helpers/skills.js';
import {onSkillClick} from '../mvc/controller.js';

/*
 * creates skill list components
 */
export function createSkills(skills, chosenSkills) {
  return skills.sort(compareSkills).map(s => createSkillListComponent(s, chosenSkills));
}

/*
 * create a single skill list component
 */
function createSkillListComponent(skill, chosenSkills) {
  const component = document.createElement('div');
  component.id = SKILL_ID_PREFIX + skill.skillId;
  component.classList.add('skill-list-skill', RARITY_MAP[skill.rarity].toLowerCase());

  if (chosenSkills && chosenSkills.some(s => s.id === skill.skillId)) {
    component.classList.add('chosen-skill-list-skill');
  }

  const skillImage = createImageNode(SKILL_IMAGES_DIR, skill.skillName);

  const footer = document.createElement('div');
  footer.classList.add('skill-list-skill-footer');

  const primaryTrait = createImageNode(TRAIT_IMAGES_DIR, TRAIT_MAP[skill.primaryTrait].name);
  primaryTrait.classList.add('skill-list-skill-primary-trait');

  const secondaryTrait = createImageNode(TRAIT_IMAGES_DIR, TRAIT_MAP[skill.secondaryTrait].name);
  secondaryTrait.classList.add('skill-list-skill-secondary-trait');

  footer.appendChild(primaryTrait);
  footer.appendChild(secondaryTrait);
  component.appendChild(createSkillInfoComponent(skill, 'skill-list-info'));
  component.appendChild(skillImage);
  component.appendChild(footer);

  component.onclick = () => onSkillClick(skill);

  return component;
}
