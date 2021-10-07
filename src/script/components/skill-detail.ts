import { LEVELS, LEVEL_OFFSET } from '@constants/eom';
import { createSkillImage, createTraitImage } from '@helpers/images';
import { hideSkillDetail, onSkillLevelClick } from '@mvc/controller';
import { Skill } from '@typez/skill.js';

const SKILL_DETAIL_CLOSE_BUTTON = createSkillDetailCloseButton();

export function createSkillDetail(skill: Skill, level: number): DocumentFragment {
  const df = new DocumentFragment();
  df.appendChild(SKILL_DETAIL_CLOSE_BUTTON);
  df.appendChild(createSkillDetailHeader(skill));
  df.appendChild(createSkillDetailMiddle(skill, level));
  df.appendChild(createSkillDetailDescription(skill, level));
  return df;
}

function createSkillDetailCloseButton(): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-detail-close');
  el.innerHTML = 'x';
  el.onclick = () => hideSkillDetail();
  return el;
}

function createSkillDetailDescription(skill: Skill, level: number): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-detail-description');
  el.innerHTML = skill.descriptions[level - LEVEL_OFFSET]?.replace(/\\n/g, '\n') ?? '';
  return el;
}

function createSkillDetailHeader(skill: Skill): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-detail-header');

  const headerRight = document.createElement('div');

  const title = document.createElement('div');
  title.innerHTML = skill.name;

  headerRight.appendChild(title);
  headerRight.appendChild(createSkillDetailTraits(skill));

  el.appendChild(createSkillImage(skill));
  el.appendChild(headerRight);

  return el;
}

function createSkillDetailTraits(skill: Skill): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-detail-traits');
  el.appendChild(createTraitImage(skill.primaryTrait));
  el.appendChild(createTraitImage(skill.secondaryTrait));
  return el;
}

/*
 * create middle component with misc data: level, skill type, cooldown
 */
function createSkillDetailMiddle(skill: Skill, level: number): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-detail-middle');
  el.appendChild(createSkillDetailLevels(level));
  el.appendChild(createSkillDetailType(skill));

  if (skill.isActive) {
    el.appendChild(createSkillDetailCooldown(skill, level));
  }

  return el;
}

function createSkillDetailLevels(skillLevel: number): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-detail-levels');

  const levelLabel = document.createElement('span');
  levelLabel.innerHTML = 'Level ';
  el.appendChild(levelLabel);

  for (const level of LEVELS) {
    const levelNumber = document.createElement('span');
    levelNumber.classList.add('level-text');
    levelNumber.classList.add(skillLevel === level ? 'active-level-text' : 'inactive-level-text');
    levelNumber.innerHTML = String(level);

    levelNumber.onclick = () => onSkillLevelClick(level);

    el.appendChild(levelNumber);
  }

  return el;
}

function createSkillDetailType(skill: Skill): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-detail-type');
  el.innerHTML = skill.isActive ? 'Active' : 'Passive';
  return el;
}

function createSkillDetailCooldown(skill: Skill, level: number): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-detail-cooldown');

  const cooldownLabel = document.createElement('span');
  cooldownLabel.innerHTML = 'CD: ';

  const cooldownNumber = document.createElement('span');
  cooldownNumber.classList.add('skill-detail-cooldown-text');
  cooldownNumber.innerHTML = skill.cooldowns[level - LEVEL_OFFSET] + 's';

  el.appendChild(cooldownLabel);
  el.appendChild(cooldownNumber);

  return el;
}
