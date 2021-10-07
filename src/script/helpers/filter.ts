import { Filter } from '@typez/filter';
import { Skill } from '@typez/skill';

export function applyFilters(filters: Filter[], skills: Skill[]): Skill[] {
  const inclusiveFilters = filters.filter(f => f.isInclusive);
  const exclusiveFilters = filters.filter(f => !f.isInclusive);

  return applyFilterSubset(exclusiveFilters, applyFilterSubset(inclusiveFilters, skills));
}

function applyFilterSubset(filters: Filter[], skills: Skill[]): Skill[] {
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
