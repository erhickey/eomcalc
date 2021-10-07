import { Compare } from '@constants/compare';
import { Skill } from '@typez/skill';
import { compareStringsCaseInsensitive } from '@util/util';

/*
 * compareFunction for skills
 *
 * order by rarity first, then by name
 */
export function compareSkills(skill1: Skill, skill2: Skill): number {
  const raritySort = compareSkillsByRarity(skill1, skill2);

  if (raritySort !== Compare.EqualTo) {
    return raritySort;
  }

  return compareStringsCaseInsensitive(skill1.name, skill2.name);
}

/*
 * compareFunction for skills by rarity
 */
export function compareSkillsByRarity(skill1: Skill, skill2: Skill): number {
  return skill2.rarityNumber - skill1.rarityNumber;
}
