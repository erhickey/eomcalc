/*
 * contains functions which create the elements indicating which skills have been chosen
 */

import {RARITY_MAP, SKILL_IMAGES_DIR, SKILL_TYPES} from '../constants/constants.js';
import {onChosenSkillClick} from '../mvc/controller.js';
import {createImageNode, compareSkills} from '../util/app-util.js';

export function createChosenSkills(skills) {
  return skills.sort(compareChosenSkills).map(s => createChosenSkillComponent(s));
}

function createChosenSkillComponent(skill) {
  const component = document.createElement('div');
  component.classList.add('chosen-skill', RARITY_MAP[skill.rarity].toLowerCase());
  component.onclick = function() { onChosenSkillClick(skill); };

  const skillImage = createImageNode(SKILL_IMAGES_DIR, skill.name);
  component.appendChild(skillImage);

  return component;
}

function compareChosenSkills(skill1, skill2) {
  if (skill1.type === skill2.type) {
    return compareSkills(skill1, skill2);
  }

  if (skill1.type === SKILL_TYPES.ACTIVE) {
    return -1;
  }

  return 1;
}
