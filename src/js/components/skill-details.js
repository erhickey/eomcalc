/*
 * contains functions which create the elements to display skill details
 */

import {HIDDEN_CLASS} from '../constants/css.js';
import {SKILL_TYPES, TRAIT_MAP} from '../constants/data.js';
import {SKILL_IMAGES_DIR, TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {createImageNode} from '../helpers/components.js';
import {format} from '../util/util.js';

/*
 * create skill details component
 */
export function createSkillDetailsComponent(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-details', HIDDEN_CLASS);

  component.appendChild(createHeaderComponent(skill));
  component.appendChild(createMiscComponent(skill));
  component.appendChild(createDescriptionComponent(skill));

  return component;
}

/*
 * create component with skill description
 */
function createDescriptionComponent(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-details-description');
  component.innerHTML = skill.descriptions[0];
  return component;
}

/*
 * create component with misc data, level, skill type, cooldown
 */
function createMiscComponent(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-details-misc');

  const level = document.createElement('div');
  level.classList.add('skill-details-misc-level');
  const levelLabel = document.createElement('span');
  levelLabel.innerHTML = 'Level ';
  const levelNumber = document.createElement('span');
  levelNumber.classList.add('skill-details-misc-dynamic-text');
  levelNumber.innerHTML = '1';

  level.appendChild(levelLabel);
  level.appendChild(levelNumber);

  const type = document.createElement('div');
  type.classList.add('skill-details-misc-type');
  type.innerHTML = SKILL_TYPES.ACTIVE === skill.skillType ? 'Active' : 'Passive';

  component.appendChild(level);
  component.appendChild(type);

  if (SKILL_TYPES.ACTIVE === skill.skillType) {
    const cooldown = document.createElement('div');
    cooldown.classList.add('skill-details-misc-cooldown');

    const cooldownLabel = document.createElement('span');
    cooldownLabel.innerHTML = 'CD: ';

    const cooldownNumber = document.createElement('span');
    cooldownNumber.classList.add('skill-details-misc-dynamic-text');
    cooldownNumber.innerHTML = skill.cooldowns[0] + 's';

    cooldown.appendChild(cooldownLabel);
    cooldown.appendChild(cooldownNumber);

    component.appendChild(cooldown);
  }

  return component;
}

/*
 * create skill detail header
 */
function createHeaderComponent(skill) {
  const header = document.createElement('div');
  header.classList.add('skill-details-header');

  const headerRight = document.createElement('div');

  const headerText = document.createElement('div');
  headerText.innerHTML = skill.skillName;

  headerRight.appendChild(headerText);
  headerRight.appendChild(createTraitsComponent(skill));

  header.appendChild(createImageNode(SKILL_IMAGES_DIR, skill.skillName));
  header.appendChild(headerRight);

  return header;
}

function createTraitsComponent(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-details-traits');

  const primaryTrait = createImageNode(TRAIT_IMAGES_DIR, TRAIT_MAP[skill.primaryTrait].name);
  primaryTrait.classList.add('skill-details-primary-trait');

  const secondaryTrait = createImageNode(TRAIT_IMAGES_DIR, TRAIT_MAP[skill.secondaryTrait].name);
  secondaryTrait.classList.add('skill-details-secondary-trait');

  component.appendChild(primaryTrait);
  component.appendChild(secondaryTrait);

  return component;
}
