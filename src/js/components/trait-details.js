/*
 * contains functions which create the elements to display trait details
 */

import {TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {createImageNode} from '../helpers/components.js';
import {hideTraitDetails} from '../mvc/controller.js';
import {format} from '../util/util.js';

/*
 * create trait details component
 */
export function createTraitDetailsComponent(trait) {
  const df = new DocumentFragment();

  const header = document.createElement('div');
  header.classList.add('trait-details-header');
  const headerText = document.createElement('span');
  headerText.innerHTML = trait.name;
  header.appendChild(createImageNode(TRAIT_IMAGES_DIR, trait.name));
  header.appendChild(headerText);

  const closeButton = document.createElement('div');
  closeButton.classList.add('trait-details-close');
  closeButton.innerHTML = 'x';
  closeButton.onclick = () => hideTraitDetails();

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

  df.appendChild(closeButton);
  df.appendChild(header);
  df.appendChild(body);

  return df;
}

/*
 * create row detailing effects of the trait at a certain breakpoint
 */
function createTraitStatsRow(breakpoint, effect, stats, isActive) {
  const row = document.createElement('div');
  row.classList.add('trait-details-bp-row');

  if (isActive) {
    row.classList.add('trait-details-bp-row-active');
  }

  row.innerHTML = breakpoint + ': ' + format(effect, stats);
  return row;
}
