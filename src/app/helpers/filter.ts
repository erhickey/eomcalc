import { Filter } from '@typez/filter';
import { Skill } from '@typez/skill';

export function applyFilters(filters: Filter[], skills: Skill[]): Skill[] {
  const inclusiveFilters = filters.filter(f => f.isInclusive);
  const exclusiveFilters = filters.filter(f => !f.isInclusive);

  return applyExclusiveFilters(exclusiveFilters, applyInclusiveFilters(inclusiveFilters, skills));
}

/*
 * apply inclusive filters, returns skills that match any of the given filters
 */
function applyInclusiveFilters(filters: Filter[], skills: Skill[]): Skill[] {
  if (!filters?.length) {
    return skills;
  }

  return skills.filter(s => {
    for (const filter of filters) {
      if (filter.match(s)) {
        return true;
      }
    }

    return false;
  });
}

/*
 * apply exclusive filters, returns skills that match all of the given filters
 */
function applyExclusiveFilters(filters: Filter[], skills: Skill[]): Skill[] {
  if (!filters?.length) {
    return skills;
  }

  let toReturn = skills;

  filters.forEach(f => {
    toReturn = toReturn.filter(s => f.match(s));
  });

  return toReturn;
}
