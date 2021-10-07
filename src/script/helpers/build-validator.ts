import { MAX_ACTIVES, MAX_PASSIVES, MAX_SKILLS } from '@constants/eom';
import { Skill } from '@typez/skill';

/*
 * validate build conforms to game rules
 */
export function validBuild(build: Skill[]): boolean {
  if (
    build.length > MAX_SKILLS ||
    build.filter(s => s.isActive).length > MAX_ACTIVES ||
    build.filter(s => !s.isActive).length > MAX_PASSIVES
  ) {
    return false;
  }

  return true;
}
