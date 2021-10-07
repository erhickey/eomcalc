import { Compare } from '@constants/compare';
import { compareStringsCaseInsensitive } from '@util/util';
import { TraitInfo } from '@typez/trait-info';

// active traits first
// then sort by number of skills with that trait
// primary traits before secondary traits
// lastly sort by name
export function compareTraits(trait1: TraitInfo, trait2: TraitInfo): number {
  const activeSort = compareTraitsByActive(trait1, trait2);

  if (activeSort !== Compare.EqualTo) {
    return activeSort;
  }

  const countSort = compareTraitsByCount(trait1, trait2);

  if (countSort !== Compare.EqualTo) {
    return countSort;
  }

  const typeSort = compareTraitsByType(trait1, trait2);

  if (typeSort !== Compare.EqualTo) {
    return typeSort;
  }

  return compareStringsCaseInsensitive(trait1.trait.name, trait2.trait.name);
}

/*
 * order traits, active traits first
 */
function compareTraitsByActive(trait1: TraitInfo, trait2: TraitInfo): number {
  if (trait1.active === trait2.active) {
    return Compare.EqualTo;
  }

  if (trait1.active) {
    return Compare.LessThan;
  }

  return Compare.GreaterThan;
}

/*
 * order traits, traits with higher counts first
 */
function compareTraitsByCount(trait1: TraitInfo, trait2: TraitInfo): number {
  if (trait1.count === trait2.count) {
    return Compare.EqualTo;
  }

  if (trait1.count > trait2.count) {
    return Compare.LessThan;
  }

  return Compare.GreaterThan;
}

/*
 * order traits, primary traits first
 */
function compareTraitsByType(trait1: TraitInfo, trait2: TraitInfo): number {
  if (trait1.trait.isPrimary === trait2.trait.isPrimary) {
    return Compare.EqualTo;
  }

  if (trait1.trait.isPrimary) {
    return Compare.LessThan;
  }

  return Compare.GreaterThan;
}
