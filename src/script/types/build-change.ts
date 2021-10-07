import { BuildChangeResult } from '@constants/results';
import { Skill } from '@typez/skill';

export interface BuildChange {
  result: BuildChangeResult;
  build: Skill[];
}

export function buildChange(build: Skill[], result: BuildChangeResult): BuildChange {
  return { build, result };
}
