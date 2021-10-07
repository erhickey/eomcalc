import { TRAITS } from '@api/eom';
import { createTraitImage } from '@helpers/images';
import { compareTraits } from '@helpers/traits';
import { onTraitClick } from '@mvc/controller';
import { Skill } from '@typez/skill';
import { TraitInfo } from '@typez/trait-info';

export function createTraits(build: Skill[]): DocumentFragment {
  const df = new DocumentFragment();

  TRAITS.map(t => new TraitInfo(t, build))
    .sort(compareTraits)
    .forEach(t => df.appendChild(createTrait(t)));

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
