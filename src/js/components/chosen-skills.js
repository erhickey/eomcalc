/*
 * contains functions which create the elements indicating which skills have been chosen
 */

import {RARITY_MAP} from '../constants/data.js';
import {SKILL_IMAGES_DIR} from '../constants/resources.js';
import {compareChosenSkills} from '../helpers/chosen-skills.js';
import {createImageNode} from '../helpers/components.js';
import {onChosenSkillClick} from '../mvc/controller.js';

/*
 * creates all chosen skill components
 */
export function createChosenSkills(skills) {
  return skills.sort(compareChosenSkills).map(s => createChosenSkillComponent(s));
}

/*
 * create a single chosen skill component
 */
function createChosenSkillComponent(skill) {
  const component = document.createElement('div');
  component.classList.add('chosen-skill', RARITY_MAP[skill.rarity].toLowerCase());
  component.onclick = () => onChosenSkillClick(skill);

  const skillImage = createImageNode(SKILL_IMAGES_DIR, skill.skillName);
  component.appendChild(skillImage);

  return component;
}
