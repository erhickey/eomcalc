/*
 * manages the model, or state, and emits changes as necessary
 */

import { BehaviorSubject, merge, Observable, ReplaySubject } from 'rxjs';
import { filter, map } from 'rxjs/operators';

import { SKILLS } from '@api/eom';
import { MAX_SKILL_LEVEL, MIN_SKILL_LEVEL } from '@constants/eom';
import { parseBuild } from '@helpers/build-text';
import { validBuild } from '@helpers/build-validator';
import { Model } from '@mvcs/model';
import { Filter } from '@typez/filter';
import { Skill } from '@typez/skill';
import { Trait } from '@typez/trait';
import { TraitInfo } from '@typez/trait-info';
import { applyFilters } from '@helpers/filter';

// size of the buffer to replay on subscription to a ReplaySubject
// value of 1 allows ReplaySubject to act like a BehaviorSubject without an initial value
const REPLAY_SUBJECT_BUFFER = 1;

export class Service {
  private model: Model = new Model([]);

  private _buildChange = new BehaviorSubject<Skill[]>([]);
  private _skillAdded = new ReplaySubject<Skill>(REPLAY_SUBJECT_BUFFER);
  private _skillRemoved = new ReplaySubject<Skill>(REPLAY_SUBJECT_BUFFER);
  private _skillLevelChange: BehaviorSubject<number>;
  private _skillDetailChange = new ReplaySubject<Skill>(REPLAY_SUBJECT_BUFFER);
  private _traitDetailChange = new ReplaySubject<TraitInfo>(REPLAY_SUBJECT_BUFFER);
  private _filtersChange = new BehaviorSubject<Filter[]>([]);
  private _visibleSkillsChange = new BehaviorSubject<Skill[]>(SKILLS);
  private _skillDetailVisibilityChange = new BehaviorSubject<boolean>(false);
  private _traitDetailVisibilityChange = new BehaviorSubject<boolean>(false);

  constructor(build: Skill[] | string) {
    this._skillLevelChange = new BehaviorSubject(this.model.skillLevel);
    this.setBuild(build);
  }

  private setBuild(build: Skill[] | string): void {
    const b = 'string' === typeof build ? parseBuild(build, SKILLS) : build;

    if (validBuild(b)) {
      this.model.build = b;
      this.buildChanged();
    }
  }

  private buildChanged(): void {
    this._buildChange.next(this.model.build);

    if (this.model.traitDetail) {
      this._traitDetailChange.next(new TraitInfo(this.model.traitDetail, this.model.build));
    }
  }

  private addSkill(skill: Skill): void {
    if (validBuild(this.model.build.concat(skill))) {
      this.model.build.push(skill);
      this._skillAdded.next(skill);
      this.buildChanged();
    }
  }

  public removeSkill(skill: Skill): void {
    if (this.buildContains(skill)) {
      this.model.build = this.model.build.filter(s => s.id !== skill.id);
      this._skillRemoved.next(skill);
      this.buildChanged();
    }
  }

  private buildContains(skill: Skill): boolean {
    return this.model.build.some(s => s.id === skill.id);
  }

  /*
   * add or remove a skill from the build
   *
   * if the skill is already in the build it will be removed
   * if adding the skill to the build results in a valid build, the skill will be added
   */
  public addOrRemoveSkill(skill: Skill): void {
    if (this.buildContains(skill)) {
      this.removeSkill(skill);
    } else {
      this.addSkill(skill);
    }
  }

  /*
   * set the level of the skill to display in skill detail
   * if the argument is outside of the allowed bounds, it will be treated as the closest bound
   */
  public setSkillLevel(level: number): void {
    const lvl = Math.max(MIN_SKILL_LEVEL, Math.min(MAX_SKILL_LEVEL, level));

    if (this.model.skillLevel !== lvl) {
      this.model.skillLevel = lvl;
      this._skillLevelChange.next(lvl);
    }
  }

  public setSkillDetail(skill: Skill): void {
    if (this.model.skillDetail?.id !== skill.id) {
      this.model.skillDetail = skill;
      this._skillDetailChange.next(skill);
      this._skillDetailVisibilityChange.next(true);
    }
  }

  public isCurrentSkillDetail(skill: Skill): boolean {
    return this.model.skillDetail?.id === skill.id;
  }

  public setTraitDetail(trait: Trait): void {
    if (this.model.traitDetail?.id !== trait.id) {
      this.model.traitDetail = trait;
      this._traitDetailChange.next(new TraitInfo(trait, this.model.build));
      this._traitDetailVisibilityChange.next(true);
    }
  }

  public isCurrentTraitDetail(trait: Trait): boolean {
    return this.model.traitDetail?.id === trait.id;
  }

  /*
   * if the filter is already active, remove it, otherwise add it to the active filters
   */
  public addOrRemoveFilter(filtr: Filter): void {
    if (this.model.filters.some(f => f.key === filtr.key)) {
      this.removeFilter(filtr);
    } else {
      this.addFilter(filtr);
    }

    this.updateFilters();
  }

  public replaceFilter(filtr: Filter): void {
    this.removeFilter(filtr);
    this.addFilter(filtr);
    this.updateFilters();
  }

  /*
   * does not trigger updates
   */
  private removeFilter(filtr: Filter): void {
    this.model.filters = this.model.filters.filter(f => f.key !== filtr.key);
  }

  /*
   * does not trigger updates
   */
  private addFilter(filtr: Filter): void {
    this.model.filters.push(filtr);
  }

  public clearFilters(): void {
    this.model.filters = [];
    this.updateFilters();
  }

  private updateFilters(): void {
    this._filtersChange.next(this.model.filters);
    this._visibleSkillsChange.next(applyFilters(this.model.filters, SKILLS));
  }

  public hideSkillDetail(): void {
    if (this._skillDetailVisibilityChange.getValue()) {
      this._skillDetailVisibilityChange.next(false);
    }
  }

  public hideTraitDetail(): void {
    if (this._traitDetailVisibilityChange.getValue()) {
      this._traitDetailVisibilityChange.next(false);
    }
  }

  public toggleSkillDetail(): void {
    this._skillDetailVisibilityChange.next(!this._skillDetailVisibilityChange.getValue());
  }

  public toggleTraitDetail(): void {
    this._traitDetailVisibilityChange.next(!this._traitDetailVisibilityChange.getValue());
  }

  get buildChange(): Observable<Skill[]> {
    return this._buildChange;
  }

  get skillAdded(): Observable<Skill> {
    return this._skillAdded;
  }

  get skillRemoved(): Observable<Skill> {
    return this._skillRemoved;
  }

  get skillLevelChange(): Observable<number> {
    return this._skillLevelChange;
  }

  get skillDetailChange(): Observable<Skill> {
    return this._skillDetailChange;
  }

  get traitDetailChange(): Observable<TraitInfo> {
    return this._traitDetailChange;
  }

  get filtersChange(): Observable<Filter[]> {
    return this._filtersChange;
  }

  get visibleSkillsChange(): Observable<Skill[]> {
    return this._visibleSkillsChange;
  }

  get skillDetailVisibilityChange(): Observable<boolean> {
    return this._skillDetailVisibilityChange;
  }

  get traitDetailVisibilityChange(): Observable<boolean> {
    return this._traitDetailVisibilityChange;
  }

  public traitChange(trait: Trait): Observable<TraitInfo> {
    return merge(this._skillAdded, this._skillRemoved)
      .pipe(filter(skill => skill.primaryTrait.id === trait.id || skill.secondaryTrait.id === trait.id))
      .pipe(map(_skill => new TraitInfo(trait, this.model.build)));
  }

  public skillChosenChange(skill: Skill): Observable<boolean> {
    return merge(this._skillAdded, this._skillRemoved)
      .pipe(filter(s => s.id === skill.id))
      .pipe(map(s => Boolean(this.model.build.find(e => e.id === s.id))));
  }

  public skillVisibilityChange(skill: Skill): Observable<boolean> {
    return this._visibleSkillsChange.pipe(map(skills => Boolean(skills.find(s => s.id === skill.id))));
  }

  get build(): Skill[] {
    return this._buildChange.getValue();
  }
}
