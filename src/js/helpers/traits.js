import {ORDER_AFTER, ORDER_BEFORE, ORDER_EQUAL} from '../constants/constants.js';
import {compareStringsCaseInsensitive, isEmpty, last} from '../util/util.js';

/*
 * helper class, consolidates info about a trait to more easily create trait components
 */
export class Trait {
  constructor(trait, skills) {
    this.name = trait.traitName;
    this.id = trait.traitId;
    this.type = 100 > trait.traitId ? 'primaryTrait' : 'secondaryTrait';
    this.description = trait.traitDescription;
    this.breakpoints = trait.traitBreakpoints;
    this.modifiers = trait.traitMods;

    // eslint-disable-next-line no-magic-numbers
    this.count = isEmpty(skills) ? 0 : skills.filter(s => s[this.type] === this.id).length;

    this.active = this.count >= this.breakpoints[0];
    this.nextBreakpoint = getNextBreakpoint(trait, this.count);
    this.currentBreakpointIndex = getCurrentBreakpointIndex(trait, this.count);
  }
}

/*
 * find the lowest breakpoint that is greater than the number of skills with the given trait
 * or the highest breakpoint if the number of skills is greater than that
 */
function getNextBreakpoint(trait, numSkillsWithTrait) {
  for (const breakpoint of trait.traitBreakpoints) {
    if (breakpoint > numSkillsWithTrait) {
      return breakpoint;
    }
  }

  return last(trait.traitBreakpoints);
}

/*
 * get the index for the current breakpoint hit for a trait, or -1 if no breakpoints have been hit
 */
function getCurrentBreakpointIndex(trait, numSkillsWithTrait) {
  let toReturn = -1;

  for (let i = 0; i < trait.traitBreakpoints.length; i++) {
    if (trait.traitBreakpoints[i] <= numSkillsWithTrait) {
      toReturn = i;
    }
  }

  return toReturn;
}

// active traits first
// then sort by number of skills with that trait
// primary traits before secondary traits
// lastly sort by name
export function compareTraits(trait1, trait2) {
  const activeSort = compareTraitsByActive(trait1, trait2);

  if (activeSort !== ORDER_EQUAL) {
    return activeSort;
  }

  const countSort = compareTraitsByCount(trait1, trait2);

  if (countSort !== ORDER_EQUAL) {
    return countSort;
  }

  const typeSort = compareTraitsByType(trait1, trait2);

  if (typeSort !== ORDER_EQUAL) {
    return typeSort;
  }

  return compareStringsCaseInsensitive(trait1.name, trait2.name);
}

/*
 * order traits, active traits first
 */
function compareTraitsByActive(trait1, trait2) {
  if (trait1.active === trait2.active) {
    return ORDER_EQUAL;
  }

  if (trait1.active) {
    return ORDER_BEFORE;
  }

  return ORDER_AFTER;
}

/*
 * order traits, traits with higher counts first
 */
function compareTraitsByCount(trait1, trait2) {
  if (trait1.count === trait2.count) {
    return ORDER_EQUAL;
  }

  if (trait1.count > trait2.count) {
    return ORDER_BEFORE;
  }

  return ORDER_AFTER;
}

/*
 * order traits, primary traits first
 */
function compareTraitsByType(trait1, trait2) {
  if (trait1.type === trait2.type) {
    return ORDER_EQUAL;
  }

  if ('primaryTrait' === trait1.type) {
    return ORDER_BEFORE;
  }

  return ORDER_AFTER;
}
