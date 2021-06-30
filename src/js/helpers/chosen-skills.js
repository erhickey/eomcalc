/**
 * helper classes/functions for creating chosen skill components
 */

import {compareSkills} from './skills.js';
import {ORDER_AFTER, ORDER_BEFORE} from '../constants/constants.js';
import {SKILL_TYPES} from '../constants/data.js';

/*
 * compareFunction for chosen skills
 *
 * order by type first (active before passive)
 * then follow the same order as the skill list
 */
export function compareChosenSkills(skill1, skill2) {
  if (skill1.type === skill2.type) {
    return compareSkills(skill1, skill2);
  }

  if (skill1.type === SKILL_TYPES.ACTIVE) {
    return ORDER_BEFORE;
  }

  return ORDER_AFTER;
}
