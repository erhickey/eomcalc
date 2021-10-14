import { combineLatestWith } from 'rxjs/operators';

import { Component } from '@components/component';
import { HIDDEN_CLASS } from '@constants/css';
import { LEVELS, LEVEL_OFFSET } from '@constants/eom';
import { SKILL_DETAIL_ID } from '@constants/html';
import { createImageElement, createSkillImageSrc, createTraitImageSrc } from '@helpers/images';
import { Controller } from '@mvcs/controller';
import { Service } from '@mvcs/service';
import { Skill } from '@typez/skill.js';

export class SkillDetailComponent extends Component {
  private skillTitle = document.createElement('div');
  private skillImage = createImageElement();
  private primaryTraitImage = createImageElement();
  private secondaryTraitImage = createImageElement();
  private levelSpans: HTMLSpanElement[];
  private skillType = SkillDetailComponent.createSkillType();
  private cooldownContainer: HTMLDivElement;
  private cooldown = SkillDetailComponent.createCooldownNumberSpan();
  private skillDescription = SkillDetailComponent.createSkillDescription();

  constructor(private controller: Controller, service: Service) {
    super();
    this.levelSpans = this.createLevelSpans();
    this.cooldownContainer = this.createCooldownContainer();
    this.render();
    this.initSubscriptions(service);
  }

  private render(): void {
    this.id = SKILL_DETAIL_ID;
    this.classList.add('skill-detail');
    this.appendChild(this.createCloseButton());
    this.appendChild(this.createHeader());
    this.appendChild(this.createMiddle());
    this.appendChild(this.skillDescription);
  }

  private initSubscriptions(service: Service): void {
    service.skillDetailChange.pipe(combineLatestWith(service.skillLevelChange)).subscribe(([skill, level]) => {
      this.update(skill, level);
    });

    service.skillDetailVisibilityChange.subscribe(isVisible => {
      if (isVisible === this.classList.contains(HIDDEN_CLASS)) {
        this.classList.toggle(HIDDEN_CLASS);
      }
    });
  }

  private update(skill: Skill, level: number): void {
    this.skillTitle.innerHTML = skill.name;
    this.skillImage.src = createSkillImageSrc(skill);
    this.primaryTraitImage.src = createTraitImageSrc(skill.primaryTrait);
    this.secondaryTraitImage.src = createTraitImageSrc(skill.secondaryTrait);
    this.skillType.innerHTML = skill.isActive ? 'Active' : 'Passive';
    this.cooldown.innerHTML = skill.cooldowns[level - LEVEL_OFFSET] + 's';
    this.skillDescription.innerHTML = skill.descriptions[level - LEVEL_OFFSET]?.replace(/\\n/g, '\n') ?? '';

    if (skill.isActive) {
      this.cooldownContainer.classList.remove(HIDDEN_CLASS);
    } else {
      this.cooldownContainer.classList.add(HIDDEN_CLASS);
    }

    this.levelSpans.forEach(levelSpan => {
      if (levelSpan.innerHTML === String(level)) {
        levelSpan.classList.add('active-level-text');
      } else {
        levelSpan.classList.remove('active-level-text');
      }
    });
  }

  private createCloseButton(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('skill-detail-close');
    el.innerHTML = 'x';
    el.onclick = () => this.controller.hideSkillDetail();
    return el;
  }

  private createHeader(): HTMLDivElement {
    const headerRight = document.createElement('div');
    headerRight.appendChild(this.skillTitle);
    headerRight.appendChild(this.createTraits());

    const el = document.createElement('div');
    el.classList.add('skill-detail-header');
    el.appendChild(this.skillImage);
    el.appendChild(headerRight);

    return el;
  }

  private createTraits(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('skill-detail-traits');
    el.appendChild(this.primaryTraitImage);
    el.appendChild(this.secondaryTraitImage);
    return el;
  }

  /*
   * create middle element with misc data: level, skill type, cooldown
   */
  private createMiddle(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('skill-detail-middle');
    el.appendChild(this.createLevels());
    el.appendChild(this.skillType);
    el.appendChild(this.cooldownContainer);
    return el;
  }

  private createLevels(): HTMLDivElement {
    const levelLabel = document.createElement('span');
    levelLabel.innerHTML = 'Level ';

    const el = document.createElement('div');
    el.classList.add('skill-detail-levels');
    el.appendChild(levelLabel);

    for (const levelSpan of this.levelSpans) {
      el.appendChild(levelSpan);
    }

    return el;
  }

  private createLevelSpans(): HTMLSpanElement[] {
    return LEVELS.map(level => {
      const el = document.createElement('span');
      el.classList.add('level-text');
      el.innerHTML = String(level);
      el.onclick = () => this.controller.onSkillLevelClick(level);
      return el;
    });
  }

  private static createSkillType(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('skill-detail-type');
    return el;
  }

  private createCooldownContainer(): HTMLDivElement {
    const cooldownLabel = document.createElement('span');
    cooldownLabel.innerHTML = 'CD: ';

    const el = document.createElement('div');
    el.classList.add('skill-detail-cooldown');
    el.appendChild(cooldownLabel);
    el.appendChild(this.cooldown);

    return el;
  }

  private static createCooldownNumberSpan(): HTMLSpanElement {
    const el = document.createElement('span');
    el.classList.add('skill-detail-cooldown-text');
    return el;
  }

  private static createSkillDescription(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('skill-detail-description');
    return el;
  }
}
