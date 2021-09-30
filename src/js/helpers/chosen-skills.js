import {compareSkills} from './skills.js';
import {ORDER_AFTER, ORDER_BEFORE} from '../constants/constants.js';

/*
 * compareFunction for chosen skills
 *
 * order by type first (active before passive)
 * then follow the same order as the skills in the skill list
 */
export function compareChosenSkills(skill1, skill2) {
  if (skill1.isActive === skill2.isActive) {
    return compareSkills(skill1, skill2);
  }

  if (skill1.isActive) {
    return ORDER_BEFORE;
  }

  return ORDER_AFTER;
}
