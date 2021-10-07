import { TRAITS } from '@api/eom';
import { createTraitImage } from '@helpers/images';
import { clearFiltersClicked, filterClicked } from '@mvc/controller';
import { Filter, SkillTypeFilter, TraitFilter } from '@typez/filter';

export const ACTIVE_FILTER = new SkillTypeFilter(true);
export const PASSIVE_FILTER = new SkillTypeFilter(false);
export const TRAIT_FILTERS = TRAITS.map(t => new TraitFilter(t));

const CLEAR_FILTERS_BUTTON = createClearFiltersButton();

export function createFilters(filters: Filter[]): DocumentFragment {
  const df = new DocumentFragment();
  df.appendChild(createFilterTop());
  df.appendChild(createTraitFilters(TRAIT_FILTERS, filters));
  df.appendChild(createTypeFilters(filters));
  return df;
}

function createTraitFilters(traitFilters: TraitFilter[], currentFilters: Filter[]): HTMLDivElement {
  const df = new DocumentFragment();

  traitFilters.forEach(tf => {
    df.appendChild(createTraitFilter(tf, currentFilters));
  });

  const el = document.createElement('div');
  el.classList.add('trait-filters');
  el.appendChild(df);
  return el;
}

function createTraitFilter(traitFilter: TraitFilter, currentFilters: Filter[]): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('trait-filter');

  if (currentFilters.some(cf => cf.key === traitFilter.key)) {
    el.classList.add('active-filter');
  }

  el.appendChild(createTraitImage(traitFilter.trait));
  el.onclick = () => filterClicked(traitFilter);
  return el;
}

function createTypeFilters(filters: Filter[]): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('type-filters');
  el.appendChild(createTypeFilter(ACTIVE_FILTER, filters, '[Active Skills]'));
  el.appendChild(createTypeFilter(PASSIVE_FILTER, filters, '[Passive Skills]'));
  return el;
}

function createTypeFilter(filter: SkillTypeFilter, currentFilters: Filter[], text: string): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('type-filter');
  el.innerHTML = text;

  if (currentFilters.some(f => f.key === filter.key)) {
    el.classList.add('active-filter');
  }

  el.onclick = () => filterClicked(filter);
  return el;
}

function createFilterTop(): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('filters-top');
  el.appendChild(CLEAR_FILTERS_BUTTON);
  return el;
}

function createClearFiltersButton(): HTMLDivElement {
  const el = document.createElement('div');
  el.classList.add('clear-filter-button');
  el.innerHTML = 'Clear All Filters';
  el.onclick = () => clearFiltersClicked();
  return el;
}
