import { compareSkills } from '@helpers/skills';
import { Compare } from '@constants/compare';
import { Skill } from '@typez/skill';

/*
 * compareFunction for build skills
 *
 * order by type first (active before passive)
 * then follow the same order as the skills in the skill list
 */
export function compareBuildSkills(skill1: Skill, skill2: Skill): number {
  if (skill1.isActive === skill2.isActive) {
    return compareSkills(skill1, skill2);
  }

  if (skill1.isActive) {
    return Compare.LessThan;
  }

  return Compare.GreaterThan;
}
