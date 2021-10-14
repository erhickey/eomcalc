import { Component } from '@components/component';
import { CHOSEN_SKILL_CLASS, HIDDEN_CLASS } from '@constants/css';
import { BUILD_SKILL_ID_PREFIX, SKILL_LIST_SKILL_ID_PREFIX } from '@constants/html';
import { createSkillImage, createTraitImage } from '@helpers/images';
import { Controller } from '@mvcs/controller';
import { Service } from '@mvcs/service';
import { Skill } from '@typez/skill';

export class SkillCardComponent extends Component {
  constructor(private skill: Skill, private controller: Controller, service: Service, private isBuildSkill = false) {
    super();
    this.render();
    this.initSubscriptions(service);
  }

  private render(): void {
    this.id = (this.isBuildSkill ? BUILD_SKILL_ID_PREFIX : SKILL_LIST_SKILL_ID_PREFIX) + this.skill.id;
    this.classList.add('skill-card', this.skill.rarity + '-card');
    this.appendChild(this.createTitle());
    this.appendChild(this.createDetailButton());
    this.appendChild(createSkillImage(this.skill));
    this.appendChild(this.createFooter());

    this.onclick = () => this.controller.onSkillClick(this.skill, this.isBuildSkill);
  }

  private initSubscriptions(service: Service): void {
    if (!this.isBuildSkill) {
      service.skillChosenChange(this.skill).subscribe(b => this.updateChosen(b));
      service.skillVisibilityChange(this.skill).subscribe(b => this.updateVisibility(b));
    }
  }

  private updateChosen(isChosen: boolean): void {
    if (isChosen) {
      this.classList.add(CHOSEN_SKILL_CLASS);
    } else {
      this.classList.remove(CHOSEN_SKILL_CLASS);
    }
  }

  private updateVisibility(isVisible: boolean): void {
    if (isVisible) {
      this.classList.remove(HIDDEN_CLASS);
    } else {
      this.classList.add(HIDDEN_CLASS);
    }
  }

  private createTitle(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('skill-card-title');
    el.innerHTML = this.skill.name;
    return el;
  }

  private createFooter(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('skill-card-footer');
    el.appendChild(createTraitImage(this.skill.primaryTrait));
    el.appendChild(createTraitImage(this.skill.secondaryTrait));
    return el;
  }

  private createDetailButton(): HTMLDivElement {
    const span = document.createElement('span');
    span.innerHTML = '?';

    const el = document.createElement('div');
    el.classList.add('skill-card-detail-button');
    el.appendChild(span);

    el.onclick = e => {
      e.stopPropagation();
      this.controller.onSkillDetailClick(this.skill);
    };

    return el;
  }
}
