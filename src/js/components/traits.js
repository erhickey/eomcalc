/*
 * contains functions which create the elements to display trait information based on chosen build
 */

import {TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {TRAITS, TRAIT_TYPES} from '../constants/data.js';
import {createImageNode} from '../helpers/components.js';
import {compareTraits, Trait} from '../helpers/traits.js';
import {onTraitClick} from '../mvc/controller.js';

/*
 * creates all trait components, with details based on selected skills
 */
export function createTraits(skills) {
  return TRAITS.map(t => new Trait(t, skills)).sort(compareTraits).map(t => createTraitComponent(t));
}

/*
 * create component displaying info about given trait
 */
function createTraitComponent(trait) {
  const component = document.createElement('div');
  component.classList.add('trait');
  component.classList.add(trait.type === TRAIT_TYPES.PRIMARY ? 'trait-primary' : 'trait-secondary');

  if (!trait.active) {
    component.classList.add('trait-inactive');
  }

  const description = document.createElement('span');
  description.innerHTML = trait.count + '/' + trait.nextBreakpoint;

  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, trait.name));
  component.appendChild(description);

  component.onclick = () => onTraitClick(component, trait);

  return component;
}
