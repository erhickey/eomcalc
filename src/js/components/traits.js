/*
 * contains functions which create the elements to display trait information based on chosen build
 */

import {createTraitDetailsComponent} from './trait-details.js';
import {HIDDEN_CLASS} from '../constants/css.js';
import {TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {TRAITS, TRAIT_TYPES} from '../constants/data.js';
import {createImageNode} from '../helpers/components.js';
import {compareTraits, Trait} from '../helpers/traits.js';
import {positionElementRelativeTo} from '../util/util.js';

// number of pixels to offset the details element by
const DETAIL_HOVER_OFFSET = 15;

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

  const details = createTraitDetailsComponent(trait);

  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, trait.name));
  component.appendChild(description);
  component.appendChild(details);

  component.addEventListener(
    'mouseenter',
    () => {
      details.classList.remove(HIDDEN_CLASS);
      details.classList.add('trait-details-visible');
      positionElementRelativeTo(component, details, DETAIL_HOVER_OFFSET);
    }
  );

  component.addEventListener(
    'mouseleave',
    () => {
      details.classList.add(HIDDEN_CLASS);
      details.classList.remove('trait-details-visible');
    }
  );

  return component;
}
