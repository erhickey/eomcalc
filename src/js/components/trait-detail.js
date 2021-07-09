import {TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {createImageNode} from '../helpers/images.js';
import {hideTraitDetail} from '../mvc/controller.js';
import {format} from '../util/util.js';

const TRAIT_DETAIL_CLOSE_BUTTON = createTraitDetailCloseButton();

export function createTraitDetail(trait) {
  const df = new DocumentFragment();
  df.appendChild(TRAIT_DETAIL_CLOSE_BUTTON);
  df.appendChild(createTraitDetailHeader(trait));
  df.appendChild(createTraitDetailBody(trait));
  return df;
}

function createTraitDetailCloseButton() {
  const component = document.createElement('div');
  component.classList.add('trait-detail-close');
  component.innerHTML = 'x';
  component.onclick = () => hideTraitDetail();
  return component;
}

function createTraitDetailHeader(trait) {
  const headerText = document.createElement('span');
  headerText.innerHTML = trait.name;

  const component = document.createElement('div');
  component.classList.add('trait-detail-header');
  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, trait.name));
  component.appendChild(headerText);
  return component;
}

function createTraitDetailBody(trait) {
  const bodyText = document.createElement('div');
  bodyText.innerHTML = trait.description;

  const component = document.createElement('div');
  component.classList.add('trait-detail-body');
  component.appendChild(bodyText);

  for (let i = 0; i < trait.stats.length; i++) {
    component.appendChild(createTraitDetailStatsRow(
      trait.breakpoints[i],
      trait.effect,
      trait.stats[i],
      i === trait.currentBreakpointIndex
    ));
  }

  return component;
}

/*
 * create row detailing effects of the trait at a certain breakpoint
 */
function createTraitDetailStatsRow(breakpoint, effect, stats, isActive) {
  const row = document.createElement('div');
  row.classList.add('trait-detail-breakpoint-row');

  if (isActive) {
    row.classList.add('trait-detail-breakpoint-row-active');
  }

  row.innerHTML = breakpoint + ': ' + format(effect, stats);
  return row;
}
