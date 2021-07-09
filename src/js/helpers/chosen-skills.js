import {compareSkills} from './skills.js';
import {ORDER_AFTER, ORDER_BEFORE} from '../constants/constants.js';
import {SKILL_TYPES} from '../constants/data.js';

/*
 * compareFunction for chosen skills
 *
 * order by type first (active before passive)
 * then follow the same order as the skills in the skill list
 */
export function compareChosenSkills(skill1, skill2) {
  if (skill1.skillType === skill2.skillType) {
    return compareSkills(skill1, skill2);
  }

  if (SKILL_TYPES.ACTIVE === skill1.skillType) {
    return ORDER_BEFORE;
  }

  return ORDER_AFTER;
}
