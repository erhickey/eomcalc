/**
 * reponsible for managing application state
 */

import { MAX_SKILL_LEVEL, MIN_SKILL_LEVEL } from '@constants/eom';
import { BuildChangeResult } from '@constants/results';
import { validBuild } from '@helpers/build-validator';
import { buildChange, BuildChange } from '@typez/build-change';
import { Filter } from '@typez/filter';
import { Skill } from '@typez/skill';
import { TraitInfo } from '@typez/trait-info';

export class Model {
  // current build
  private build: Skill[] = [];

  // skill detail component's current trait
  private skillDetail: Skill | undefined;

  // trait detail component's current trait
  private traitDetail: TraitInfo | undefined;

  // active filters
  private filters: Filter[] = [];

  // level of skill to display in skill detail component
  private skillLevel = MIN_SKILL_LEVEL;

  // initialize the model
  // set the current build, if the supplied build is valid
  // returns the current build
  public initialize(build: Skill[]): Skill[] {
    if (validBuild(build)) {
      this.build = build;
    }

    return this.build;
  }

  public getBuild(): Skill[] {
    return this.build;
  }

  /*
   * true if the skill is in the current build
   */
  public buildHasSkill(skill: Skill): boolean {
    return this.build.some(s => s.id === skill.id);
  }

  /*
   * add or remove a skill from the build
   *
   * if the skill is already in the build it will be removed
   * if adding the skill to the build results in a valid build, the skill will be added
   * no change to the build will be made if the resulting build is invalid
   */
  public addOrRemoveSkill(skill: Skill): BuildChange {
    if (this.buildHasSkill(skill)) {
      return this.removeSkill(skill);
    }

    if (validBuild(this.build.concat(skill))) {
      this.build.push(skill);
      return buildChange(this.build, BuildChangeResult.SkillAdded);
    }

    return buildChange(this.build, BuildChangeResult.NoChange);
  }

  /*
   * remove skill from the build
   */
  public removeSkill(skill: Skill): BuildChange {
    if (this.buildHasSkill(skill)) {
      this.build = this.build.filter(s => s.id !== skill.id);
      return buildChange(this.build, BuildChangeResult.SkillRemoved);
    }

    return buildChange(this.build, BuildChangeResult.NoChange);
  }

  /*
   * set the level of the skill to display in skill detail
   * if the argument is outside of the allowed bounds, it will be
   * treated as the closest bound
   *
   * return value indicates if the level changed
   */
  public setSkillLevel(level: number): boolean {
    let lvl = level > MAX_SKILL_LEVEL ? MAX_SKILL_LEVEL : level;
    lvl = level < MIN_SKILL_LEVEL ? MIN_SKILL_LEVEL : level;

    if (this.skillLevel === lvl) {
      return false;
    }

    this.skillLevel = lvl;
    return true;
  }

  public getSkillLevel(): number {
    return this.skillLevel;
  }

  /*
   * update the skill to display in skill detail
   * return value indicates if the skill changed
   */
  public updateSkillDetail(skill: Skill): boolean {
    if (this.skillDetail?.id === skill.id) {
      return false;
    }

    this.skillDetail = skill;
    return true;
  }

  public getSkillDetail(): Skill | undefined {
    return this.skillDetail;
  }

  /*
   * update the trait to display in trait detail
   * return value indicates if the trait changed
   * returns false if the trait didn't change
   */
  public updateTraitDetail(trait: TraitInfo): boolean {
    if (this.traitDetail?.trait.id === trait.trait.id) {
      return false;
    }

    this.traitDetail = trait;
    return true;
  }

  /*
   * if the filter is already active, remove it, otherwise add it to the active filters
   * returns the modified filters
   */
  public addOrRemoveFilter(filter: Filter): Filter[] {
    if (this.filters.some(f => f.key === filter.key)) {
      this.filters = this.filters.filter(f => f.key !== filter.key);
    } else {
      this.filters.push(filter);
    }

    return this.filters;
  }

  /*
   * clear currentFilters
   */
  public clearFilters(): void {
    this.filters = [];
  }
}
