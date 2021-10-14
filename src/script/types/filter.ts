import { Skill } from '@typez/skill';
import { Trait } from '@typez/trait';

export abstract class Filter {
  constructor(private _key: string, private _isInclusive: boolean) {}

  abstract match(skill: Skill): boolean;

  get key(): string {
    return this._key;
  }

  get isInclusive(): boolean {
    return this._isInclusive;
  }
}

export class SkillTypeFilter extends Filter {
  constructor(private _isActive: boolean) {
    super('skill-type-' + _isActive, false);
  }

  match(skill: Skill): boolean {
    return skill.isActive === this._isActive;
  }

  get isActive(): boolean {
    return this._isActive;
  }
}

export class TraitFilter extends Filter {
  constructor(private _trait: Trait) {
    super('trait' + _trait.id, true);
  }

  match(skill: Skill): boolean {
    return this._trait.isPrimary
      ? skill.primaryTrait.id === this._trait.id
      : skill.secondaryTrait.id === this._trait.id;
  }

  get trait(): Trait {
    return this._trait;
  }
}
