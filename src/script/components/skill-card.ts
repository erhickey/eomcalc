import { CHOSEN_SKILL_CLASS } from '@constants/css';
import { createSkillImage, createTraitImage } from '@helpers/images';
import { onSkillClick, onSkillDetailClick } from '@mvc/controller';
import { Skill } from '@typez/skill';

export function createSkillCards(skills: Skill[], idPrefix: string, build: Skill[] = []): DocumentFragment {
  const buildSkillIds = build.map(bs => bs.id);
  const df = new DocumentFragment();
  skills.forEach(s => df.appendChild(createSkillCard(s, buildSkillIds, idPrefix)));
  return df;
}

function createSkillCard(skill: Skill, buildIds: number[], idPrefix: string): HTMLDivElement {
  const el = document.createElement('div');
  el.id = idPrefix + skill.id;
  el.classList.add('skill-card', skill.rarity + '-card');

  if (buildIds.includes(skill.id)) {
    el.classList.add(CHOSEN_SKILL_CLASS);
  }

  el.appendChild(createSkillCardTitle(skill));
  el.appendChild(createSkillDetailButton(skill));
  el.appendChild(createSkillImage(skill));
  el.appendChild(createSkillCardFooter(skill));

  el.onclick = () => onSkillClick(skill);

  return el;
}

function createSkillCardTitle(skill: Skill): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-card-title');
  el.innerHTML = skill.name;
  return el;
}

function createSkillCardFooter(skill: Skill): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-card-footer');
  el.appendChild(createTraitImage(skill.primaryTrait));
  el.appendChild(createTraitImage(skill.secondaryTrait));
  return el;
}

function createSkillDetailButton(skill: Skill): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('skill-card-detail-button');

  const span = document.createElement('span');
  span.innerHTML = '?';

  el.appendChild(span);

  el.onclick = e => {
    e.stopPropagation();
    onSkillDetailClick(skill);
  };

  return el;
}
