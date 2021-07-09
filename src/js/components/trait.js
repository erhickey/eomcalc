import {TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {TRAITS} from '../constants/data.js';
import {createImageNode} from '../helpers/images.js';
import {compareTraits, Trait} from '../helpers/traits.js';
import {onTraitClick} from '../mvc/controller.js';

/*
 * creates all trait components and orders them
 */
export function createTraits(skills) {
  return TRAITS.map(t => new Trait(t, skills)).sort(compareTraits).map(t => createTrait(t));
}

function createTrait(trait) {
  const component = document.createElement('div');
  component.classList.add('trait');

  if (!trait.active) {
    component.classList.add('trait-inactive');
  }

  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, trait.name));
  component.appendChild(createTraitDescription(trait));

  component.onclick = () => onTraitClick(component, trait);

  return component;
}

function createTraitDescription(trait) {
  const component = document.createElement('span');
  component.innerHTML = trait.count + '/' + trait.nextBreakpoint;
  return component;
}
