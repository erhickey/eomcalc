/*
 * contains functions which create the elements to display trait information based on chosen build
 */

import {TRAITS, TRAIT_IMAGES_DIR, TRAIT_TYPES} from '../constants/constants.js';
import {createImageNode} from '../util/app-util.js';

export function createTraits(skills) {
  return TRAITS.map(t => {
    const [count, breakpoint] = getTraitQuantity(t, skills);
    return {
      'name': t.name,
      'type': t.type,
      'active': count >= t.breakpoints[0],
      count,
      breakpoint
    };
  }).sort(compareTraits).map(t => createTraitComponent(t));
}

function getTraitQuantity(trait, skills) {
  const type = trait.type === TRAIT_TYPES.PRIMARY ? 'primaryTrait' : 'secondaryTrait';
  const count = skills ? skills.filter(s => s[type] === trait.id).length : 0;
  if (count > 0) { console.log(count);console.log(trait); }
  const breakpointCandidates = trait.breakpoints.filter(bp => bp >= count);
  // use highest breakpoint if the number of skills with this trait exceeds it
  let breakpoint = trait.breakpoints[trait.breakpoints.length - 1];

  if (breakpointCandidates.length > 1) {
    // if number of skills equals a breakpoint, use the next highest
    breakpoint = breakpointCandidates[0] === count ? breakpointCandidates[1] : breakpointCandidates[0];
  } else if (breakpointCandidates === 1) {
    breakpoint = breakpointCandidates[0];
  }

  return [count, breakpoint];
}

function createTraitComponent(trait) {
  const component = document.createElement('div');
  component.classList.add('trait');
  component.classList.add(trait.type === TRAIT_TYPES.PRIMARY ? 'trait-primary' : 'trait-secondary');

  if (!trait.active) {
    component.classList.add('trait-inactive');
  }

  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, trait.name));

  const textEl = document.createElement('span');
  textEl.innerHTML = trait.count + '/' + trait.breakpoint;
  component.appendChild(textEl);

  return component;
}

function compareTraits(trait1, trait2) {
  if (!trait1.active || !trait2.active) {
    if (trait1.active) {
      return -1;
    }

    if (trait2.active) {
      return 1;
    }
  }

  if (trait1.count === trait2.count) {
    if (trait1.type === trait2.type) {
      return compareTraitsByName(trait1, trait2);
    }

    if (trait1.type === TRAIT_TYPES.PRIMARY) {
      return -1;
    }

    return 1;
  }

  return trait2.count - trait1.count;
}

function compareTraitsByName(trait1, trait2) {
  if (trait1.name.toUpperCase() < trait2.name.toUpperCase()) {
    return -1;
  }

  return 1;
}
