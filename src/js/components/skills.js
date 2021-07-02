/*
 * contains functions which create the elements that make up the list of skills to choose from
 */

import {createSkillDetailsComponent} from './skill-details.js';
import {HIDDEN_CLASS} from '../constants/css.js';
import {RARITY_MAP, TRAIT_MAP} from '../constants/data.js';
import {SKILL_ID_PREFIX} from '../constants/html.js';
import {SKILL_IMAGES_DIR, TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {createImageNode} from '../helpers/components.js';
import {compareSkills} from '../helpers/skills.js';
import {onSkillClick} from '../mvc/controller.js';
import {positionElementRelativeTo} from '../util/util.js';

// number of pixels to offset the details element by
const DETAIL_HOVER_OFFSET = 12;

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
  component.id = SKILL_ID_PREFIX + skill.id;
  component.classList.add('skill-list-skill', RARITY_MAP[skill.rarity].toLowerCase());

  if (chosenSkills && chosenSkills.some(s => s.id === skill.id)) {
    component.classList.add('chosen-skill-list-skill');
  }

  const skillImage = createImageNode(SKILL_IMAGES_DIR, skill.name);

  const footer = document.createElement('div');
  footer.classList.add('skill-list-skill-footer');

  const primaryTrait = createImageNode(TRAIT_IMAGES_DIR, TRAIT_MAP[skill.primaryTrait].name);
  primaryTrait.classList.add('skill-list-skill-primary-trait');

  const secondaryTrait = createImageNode(TRAIT_IMAGES_DIR, TRAIT_MAP[skill.secondaryTrait].name);
  secondaryTrait.classList.add('skill-list-skill-secondary-trait');

  const details = createSkillDetailsComponent(skill);

  footer.appendChild(primaryTrait);
  footer.appendChild(secondaryTrait);
  component.appendChild(createInfoComponent(details));
  component.appendChild(skillImage);
  component.appendChild(footer);
  component.appendChild(details);

  component.onclick = () => onSkillClick(skill);

  return component;
}

function createInfoComponent(details) {
  const component = document.createElement('div');
  component.classList.add('skill-list-info');
  const span = document.createElement('span');
  span.innerHTML = '?';
  component.appendChild(span);

  component.addEventListener(
    'mouseenter',
    () => {
      details.classList.remove(HIDDEN_CLASS);
      details.classList.add('skill-details-visible');
      positionElementRelativeTo(component, details, DETAIL_HOVER_OFFSET);
    }
  );

  component.addEventListener(
    'mouseleave',
    () => {
      details.classList.add(HIDDEN_CLASS);
      details.classList.remove('skill-details-visible');
    }
  );

  component.onclick = (e) => e.stopPropagation();

  return component;
}
