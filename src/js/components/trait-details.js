/*
 * contains functions which create the elements to display trait details
 */

import {HIDDEN_CLASS} from '../constants/css.js';
import {TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {createImageNode} from '../helpers/components.js';
import {format} from '../util/util.js';

/*
 * create trait details component
 */
export function createTraitDetailsComponent(trait) {
  const component = document.createElement('div');
  component.classList.add('trait-details', HIDDEN_CLASS);

  const header = document.createElement('div');
  header.classList.add('trait-details-header');
  const headerText = document.createElement('span');
  headerText.innerHTML = trait.name;
  header.appendChild(createImageNode(TRAIT_IMAGES_DIR, trait.name));
  header.appendChild(headerText);

  const body = document.createElement('div');
  body.classList.add('trait-details-body');
  const bodyText = document.createElement('div');
  bodyText.innerHTML = trait.description;
  body.appendChild(bodyText);

  for (let i = 0; i < trait.stats.length; i++) {
    body.appendChild(createTraitStatsRow(
      trait.breakpoints[i],
      trait.effect,
      trait.stats[i],
      i === trait.currentBreakpointIndex
    ));
  }

  component.appendChild(header);
  component.appendChild(body);

  return component;
}

function createTraitStatsRow(breakpoint, effect, stats, isActive) {
  const row = document.createElement('div');
  row.classList.add('trait-details-bp-row');

  if (isActive) {
    row.classList.add('trait-details-bp-row-active');
  }

  row.innerHTML = breakpoint + ': ' + format(effect, stats);
  return row;
}
