import {LEVELS, SKILL_TYPES, TRAIT_MAP} from '../constants/data.js';
import {SKILL_IMAGES_DIR, TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {createImageNode} from '../helpers/images.js';
import {hideSkillDetail, onSkillLevelClick} from '../mvc/controller.js';

const SKILL_DETAIL_CLOSE_BUTTON = createSkillDetailCloseButton();

export function createSkillDetail(skill, level) {
  const df = new DocumentFragment();
  df.appendChild(SKILL_DETAIL_CLOSE_BUTTON);
  df.appendChild(createSkillDetailHeader(skill));
  df.appendChild(createSkillDetailMiddle(skill, level));
  df.appendChild(createSkillDetailDescription(skill, level));
  return df;
}

function createSkillDetailCloseButton() {
  const component = document.createElement('div');
  component.classList.add('skill-detail-close');
  component.innerHTML = 'x';
  component.onclick = () => hideSkillDetail();
  return component;
}

function createSkillDetailDescription(skill, level) {
  const component = document.createElement('div');
  component.classList.add('skill-detail-description');
  // eslint-disable-next-line no-magic-numbers
  component.innerHTML = skill.descriptions[level - 1].replace(/\\n/g, '\n');
  return component;
}

function createSkillDetailHeader(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-detail-header');

  const headerRight = document.createElement('div');

  const title = document.createElement('div');
  title.innerHTML = skill.skillName;

  headerRight.appendChild(title);
  headerRight.appendChild(createSkillDetailTraits(skill));

  component.appendChild(createImageNode(SKILL_IMAGES_DIR, skill.skillName));
  component.appendChild(headerRight);

  return component;
}

function createSkillDetailTraits(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-detail-traits');
  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, TRAIT_MAP[skill.primaryTrait].name));
  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, TRAIT_MAP[skill.secondaryTrait].name));
  return component;
}

/*
 * create middle component with misc data: level, skill type, cooldown
 */
function createSkillDetailMiddle(skill, level) {
  const component = document.createElement('div');
  component.classList.add('skill-detail-middle');
  component.appendChild(createSkillDetailLevels(skill, level));
  component.appendChild(createSkillDetailType(skill));

  if (SKILL_TYPES.ACTIVE === skill.skillType) {
    component.appendChild(createSkillDetailCooldown(skill, level));
  }

  return component;
}

function createSkillDetailLevels(skill, level) {
  const component = document.createElement('div');
  component.classList.add('skill-detail-levels');

  const levelLabel = document.createElement('span');
  levelLabel.innerHTML = 'Level ';
  component.appendChild(levelLabel);

  for (let i = 0; i < LEVELS.length; i++) {
    const levelNumber = document.createElement('span');
    levelNumber.classList.add('level-text');
    levelNumber.classList.add(LEVELS[i] === level ? 'active-level-text' : 'inactive-level-text');
    levelNumber.innerHTML = LEVELS[i];

    levelNumber.onclick = () => onSkillLevelClick(LEVELS[i]);

    component.appendChild(levelNumber);
  }

  return component;
}

function createSkillDetailType(skill) {
  const component = document.createElement('div');
  component.classList.add('skill-detail-type');
  component.innerHTML = SKILL_TYPES.ACTIVE === skill.skillType ? 'Active' : 'Passive';
  return component;
}

function createSkillDetailCooldown(skill, level) {
  const component = document.createElement('div');
  component.classList.add('skill-detail-cooldown');

  const cooldownLabel = document.createElement('span');
  cooldownLabel.innerHTML = 'CD: ';

  const cooldownNumber = document.createElement('span');
  cooldownNumber.classList.add('skill-detail-cooldown-text');
  // eslint-disable-next-line no-magic-numbers
  cooldownNumber.innerHTML = skill.cooldowns[level - 1] + 's';

  component.appendChild(cooldownLabel);
  component.appendChild(cooldownNumber);

  return component;
}
