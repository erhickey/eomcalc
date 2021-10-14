import { SKILLS } from '@api/eom';
import { Component } from '@components/component';
import { FiltersComponent } from '@components/filters';
import { SkillCardComponent } from '@components/skill-card';
import { SKILLS_AND_FILTERS_CONTAINER_ID, SKILL_LIST_CONTAINER_ID } from '@constants/html';
import { compareSkills } from '@helpers/comparators';
import { Controller } from '@mvcs/controller';
import { Service } from '@mvcs/service';

export class SkillListAndFiltersComponent extends Component {
  constructor(private controller: Controller, service: Service) {
    super();
    this.render(service);
  }

  private render(service: Service): void {
    this.id = SKILLS_AND_FILTERS_CONTAINER_ID;
    this.appendChild(new FiltersComponent(this.controller, service).element);
    this.appendChild(this.createSkillList(service));
  }

  private createSkillList(service: Service): HTMLDivElement {
    const el = document.createElement('div');
    el.id = SKILL_LIST_CONTAINER_ID;

    SKILLS.sort(compareSkills).forEach(skill => {
      el.appendChild(new SkillCardComponent(skill, this.controller, service).element);
    });

    return el;
  }
}
