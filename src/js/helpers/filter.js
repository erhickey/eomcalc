import {isEmpty} from '../util/util.js';

export function applyFilters(filters, skills) {
  if (isEmpty(filters)) {
    return skills;
  }

  const skillTypeFilters = filters.filter(f => 'skillType' === f.key);
  const traitFilters = filters.filter(f => 'primaryTrait' === f.key || 'secondaryTrait' === f.key);

  return applyFilterSubset(traitFilters, applyFilterSubset(skillTypeFilters, skills));
}

function applyFilterSubset(filters, skills) {
  if (isEmpty(filters)) {
    return skills;
  }

  return skills.filter(s => {
    for (const filter of filters) {
      if (s[filter.key] === filter.value) {
        return true;
      }
    }

    return false;
  });
}
