import { createTraitImage } from '@helpers/images';
import { hideTraitDetail } from '@mvc/controller';
import { TraitInfo } from '@typez/trait-info';

const TRAIT_DETAIL_CLOSE_BUTTON = createTraitDetailCloseButton();

export function createTraitDetail(trait: TraitInfo): DocumentFragment {
  const df = new DocumentFragment();
  df.appendChild(TRAIT_DETAIL_CLOSE_BUTTON);
  df.appendChild(createTraitDetailHeader(trait));
  df.appendChild(createTraitDetailBody(trait));
  return df;
}

function createTraitDetailCloseButton(): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('trait-detail-close');
  el.innerHTML = 'x';
  el.onclick = () => hideTraitDetail();
  return el;
}

function createTraitDetailHeader(trait: TraitInfo): HTMLDivElement {
  const headerText = document.createElement('span');
  headerText.innerHTML = trait.trait.name;

  const el = document.createElement('div');
  el.classList.add('trait-detail-header');
  el.appendChild(createTraitImage(trait.trait));
  el.appendChild(headerText);
  return el;
}

function createTraitDetailBody(trait: TraitInfo): HTMLDivElement {
  const bodyText = document.createElement('div');
  bodyText.innerHTML = trait.trait.description;

  const el = document.createElement('div');
  el.classList.add('trait-detail-body');
  el.appendChild(bodyText);

  for (let i = 0; i < trait.trait.mods.length; i++) {
    el.appendChild(
      createTraitDetailStatsRow(trait.trait.breakpoints[i], trait.trait.mods[i], i === trait.currentBreakpointIndex)
    );
  }

  return el;
}

/*
 * create row detailing effects of the trait at a certain breakpoint
 */
function createTraitDetailStatsRow(
  breakpoint: number | undefined,
  effect: string | undefined,
  isActive: boolean
): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('trait-detail-breakpoint-row');

  if (isActive) {
    el.classList.add('trait-detail-breakpoint-row-active');
  }

  el.innerHTML = breakpoint + ': ' + effect;
  return el;
}
