import { MIN_SKILL_LEVEL } from '@constants/eom';
import { Filter } from '@typez/filter';
import { Skill } from '@typez/skill';
import { Trait } from '@typez/trait';

export class Model {
  // skill detail component's current trait
  skillDetail: Skill | undefined;

  // trait detail component's current trait
  traitDetail: Trait | undefined;

  // active filters
  filters: Filter[] = [];

  // level of skill to display in skill detail component
  skillLevel = MIN_SKILL_LEVEL;

  constructor(public build: Skill[]) {}
}
