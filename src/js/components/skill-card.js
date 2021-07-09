import {CHOSEN_SKILL_CLASS} from '../constants/css.js';
import {RARITY_MAP, TRAIT_MAP} from '../constants/data.js';
import {SKILL_ID_PREFIX} from '../constants/html.js';
import {SKILL_IMAGES_DIR, TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {createImageNode} from '../helpers/images.js';
import {onSkillClick, onSkillDetailClick} from '../mvc/controller.js';

export function createSkillCards(skills, idPrefix, chosenSkills = []) {
  return skills.map(s => createSkillCard(s, idPrefix, chosenSkills));
}

/*
 * create a single skill card
 */
function createSkillCard(skill, idPrefix, chosenSkills) {
  const component = document.createElement('div');
  component.id = idPrefix + SKILL_ID_PREFIX + skill.skillId;
  component.classList.add('skill-card', RARITY_MAP[skill.rarity].toLowerCase() + '-card');

  if (chosenSkills && chosenSkills.some(s => s.skillId === skill.skillId)) {
    component.classList.add(CHOSEN_SKILL_CLASS);
  }

  component.appendChild(createSkillCardTitle(skill));
  component.appendChild(createSkillDetailButton(skill, 'skill-card-info'));
  component.appendChild(createImageNode(SKILL_IMAGES_DIR, skill.skillName));
  component.appendChild(createSkillCardFooter(skill));

  component.onclick = () => onSkillClick(skill);

  return component;
}

function createSkillCardTitle(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-card-title');
  component.innerHTML = skill.skillName;
  return component;
}

function createSkillCardFooter(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-card-footer');
  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, TRAIT_MAP[skill.primaryTrait].name));
  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, TRAIT_MAP[skill.secondaryTrait].name));
  return component;
}

function createSkillDetailButton(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-card-detail-button');

  const span = document.createElement('span');
  span.innerHTML = '?';

  component.appendChild(span);

  component.onclick = (e) => {
    e.stopPropagation();
    onSkillDetailClick(skill);
  };

  return component;
}
