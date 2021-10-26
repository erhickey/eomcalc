/*
 * responsible for handling user input
 */

import { ESCAPE_KEY } from '@constants/keys';
import { Service } from '@mvcs/service';
import { Filter } from '@typez/filter';
import { Skill } from '@typez/skill';
import { Trait } from '@typez/trait';

export class Controller {
  constructor(private service: Service) {
    // hide detail components when esc is pressed
    document.onkeydown = event => {
      if (ESCAPE_KEY === event.key) {
        service.hideSkillDetail();
        service.hideTraitDetail();
      }
    };
  }

  public onSkillClick(skill: Skill, isBuildSkill: boolean): void {
    if (isBuildSkill) {
      this.service.removeSkill(skill);
    } else {
      this.service.addOrRemoveSkill(skill);
    }
  }

  public onSkillDetailClick(skill: Skill): void {
    if (this.service.isCurrentSkillDetail(skill)) {
      this.service.toggleSkillDetail();
    } else {
      this.service.setSkillDetail(skill);
    }
  }

  public hideSkillDetail(): void {
    this.service.hideSkillDetail();
  }

  public onSkillLevelClick(level: number): void {
    this.service.setSkillLevel(level);
  }

  public onTraitClick(trait: Trait): void {
    if (this.service.isCurrentTraitDetail(trait)) {
      this.service.toggleTraitDetail();
    } else {
      this.service.setTraitDetail(trait);
    }
  }

  public hideTraitDetail(): void {
    this.service.hideTraitDetail();
  }

  public onFilterClick(filter: Filter): void {
    this.service.addOrRemoveFilter(filter);
  }

  public onTextFilterChange(filter: Filter): void {
    this.service.replaceFilter(filter);
  }

  public onClearFiltersClick(): void {
    this.service.clearFilters();
  }
}
