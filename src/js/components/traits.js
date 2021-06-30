/*
 * contains functions which create the elements to display trait information based on chosen build
 */

import {ORDER_AFTER, ORDER_BEFORE, ORDER_EQUAL, TRAITS, TRAIT_IMAGES_DIR, TRAIT_TYPES} from '../constants/constants.js';
import {createImageNode} from '../util/app-util.js';
import {compareStringsCaseInsensitive, isEmpty, last} from '../util/util.js';

class Trait {
  constructor(trait, skills) {
    this.name = trait.name;
    this.type = trait.type;
    const type = trait.type === TRAIT_TYPES.PRIMARY ? 'primaryTrait' : 'secondaryTrait';
    // eslint-disable-next-line no-magic-numbers
    this.count = isEmpty(skills) ? 0 : skills.filter(s => s[type] === trait.id).length;
    this.active = this.count >= trait.breakpoints[0];
    this.breakpoint = getNextBreakpoint(trait, this.count);
  }
}

export function createTraits(skills) {
  return TRAITS.map(t => new Trait(t, skills)).sort(compareTraits).map(t => createTraitComponent(t));
}

/*
 * find the lowest breakpoint that is greater than the number of skills with the given trait
 * or the highest breakpoint if the number of skills is greater than that
 */
function getNextBreakpoint(trait, numSkillsWithTrait) {
  for (const breakpoint of trait.breakpoints) {
    if (breakpoint > numSkillsWithTrait) {
      return breakpoint;
    }
  }

  return last(trait.breakpoints);
}

function createTraitComponent(trait) {
  const component = document.createElement('div');
  component.classList.add('trait');
  component.classList.add(trait.type === TRAIT_TYPES.PRIMARY ? 'trait-primary' : 'trait-secondary');

  if (!trait.active) {
    component.classList.add('trait-inactive');
  }

  const textEl = document.createElement('span');
  textEl.innerHTML = trait.count + '/' + trait.breakpoint;

  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, trait.name));
  component.appendChild(textEl);

  return component;
}

// active traits first
// then sort by number of skills with that trait
// primary traits before secondary traits
// lastly sort by name
function compareTraits(trait1, trait2) {
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

function compareTraitsByActive(trait1, trait2) {
  if (trait1.active === trait2.active) {
    return ORDER_EQUAL;
  }

  if (trait1.active) {
    return ORDER_BEFORE;
  }

  return ORDER_AFTER;
}

function compareTraitsByCount(trait1, trait2) {
  if (trait1.count === trait2.count) {
    return ORDER_EQUAL;
  }

  if (trait1.count > trait2.count) {
    return ORDER_BEFORE;
  }

  return ORDER_AFTER;
}

function compareTraitsByType(trait1, trait2) {
  if (trait1.type === trait2.type) {
    return ORDER_EQUAL;
  }

  if (trait1.type === TRAIT_TYPES.PRIMARY) {
    return ORDER_BEFORE;
  }

  return ORDER_AFTER;
}
