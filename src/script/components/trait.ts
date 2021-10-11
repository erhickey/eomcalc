import { createTraitImage } from '@helpers/images';
import { onTraitClick } from '@mvc/controller';
import { TraitInfo } from '@typez/trait-info';

export function createTraits(traits: TraitInfo[]): DocumentFragment {
  const df = new DocumentFragment();
  traits.forEach(t => df.appendChild(createTrait(t)));
  return df;
}

function createTrait(trait: TraitInfo): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('trait');

  if (!trait.active) {
    el.classList.add('trait-inactive');
  }

  el.appendChild(createTraitImage(trait.trait));
  el.appendChild(createTraitDescription(trait));

  el.onclick = () => onTraitClick(el, trait);

  return el;
}

function createTraitDescription(trait: TraitInfo): HTMLSpanElement {
  const el = document.createElement('span');
  el.innerHTML = trait.count + '/' + trait.nextBreakpoint;
  return el;
}
