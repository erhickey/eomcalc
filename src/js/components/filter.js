import {SKILL_TYPES} from '../constants/data.js';
import {ACTIVE_FILTER, PASSIVE_FILTER, TRAIT_FILTERS} from '../constants/filters.js';
import {TRAIT_IMAGES_DIR} from '../constants/resources.js';
import {createImageNode} from '../helpers/images.js';
import {clearFiltersClicked, filterClicked} from '../mvc/controller.js';
import {appendChildren} from '../util/util.js';

const CLEAR_FILTERS_BUTTON = createClearFiltersButton();

export function createFilters(currentFilters = []) {
  const filtersTop = document.createElement('div');
  filtersTop.classList.add('filters-top');
  filtersTop.appendChild(CLEAR_FILTERS_BUTTON);

  const traitFilters = document.createElement('div');
  traitFilters.classList.add('trait-filters');
  appendChildren(traitFilters, TRAIT_FILTERS.map(tf => createTraitFilter(tf, currentFilters)));

  const typeFilters = document.createElement('div');
  typeFilters.classList.add('type-filters');
  typeFilters.appendChild(createActiveFilter(currentFilters));
  typeFilters.appendChild(createPassiveFilter(currentFilters));

  return [filtersTop, traitFilters, typeFilters];
}

function createTraitFilter(traitFilter, currentFilters) {
  const component = document.createElement('div');
  component.classList.add('trait-filter');

  if (currentFilters
    .some(cf => ('primaryTrait' === cf.key || 'secondaryTrait' === cf.key) && cf.value === traitFilter.value)
  ) {
    component.classList.add('active-filter');
  }

  component.appendChild(createImageNode(TRAIT_IMAGES_DIR, traitFilter.trait.name));
  component.onclick = () => filterClicked(traitFilter);
  return component;
}

function createActiveFilter(currentFilters) {
  const component = document.createElement('div');
  component.classList.add('type-filter');
  component.innerHTML = '[Active Skills]';

  if (currentFilters.some(cf => 'skillType' === cf.key && SKILL_TYPES.ACTIVE === cf.value)) {
    component.classList.add('active-filter');
  }

  component.onclick = () => filterClicked(ACTIVE_FILTER);
  return component;
}

function createPassiveFilter(currentFilters) {
  const component = document.createElement('div');
  component.classList.add('type-filter');
  component.innerHTML = '[Passive Skills]';

  if (currentFilters.some(cf => 'skillType' === cf.key && SKILL_TYPES.PASSIVE === cf.value)) {
    component.classList.add('active-filter');
  }

  component.onclick = () => filterClicked(PASSIVE_FILTER);
  return component;
}

function createClearFiltersButton() {
  const component = document.createElement('div');
  component.classList.add('clear-filter-button');
  component.innerHTML = 'Clear All Filters';
  component.onclick = () => clearFiltersClicked();
  return component;
}
