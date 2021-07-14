import {MAX_ACTIVES, MAX_PASSIVES, MAX_SKILLS} from '../constants/app.js';
import {SKILL_TYPES} from '../constants/data.js';

/*
 * validate build conforms to game rules
 */
export function validBuild(build) {
  if (
    build.length > MAX_SKILLS
    || build.filter(s => s.skillType === SKILL_TYPES.ACTIVE).length > MAX_ACTIVES
    || build.filter(s => s.skillType === SKILL_TYPES.PASSIVE).length > MAX_PASSIVES
  ) {
    return false;
  }

  return true;
}
