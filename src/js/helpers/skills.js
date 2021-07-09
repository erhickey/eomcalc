import {ORDER_EQUAL} from '../constants/constants.js';
import {compareStringsCaseInsensitive} from '../util/util.js';

/*
 * compareFunction for skills
 *
 * order by rarity first, then by name
 */
export function compareSkills(skill1, skill2) {
  const raritySort = compareSkillsByRarity(skill1, skill2);

  if (raritySort !== ORDER_EQUAL) {
    return raritySort;
  }

  return compareStringsCaseInsensitive(skill1.skillName, skill2.skillName);
}

/*
 * compareFunction for skills by rarity
 */
export function compareSkillsByRarity(skill1, skill2) {
  return skill2.rarity - skill1.rarity;
}
