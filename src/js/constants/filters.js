/*
 * filters to narrow down list of skills
 */

import {TRAITS} from './data.js';

export const TRAIT_FILTERS = TRAITS.map(t => ({
  key: 100 > t.traitId ? 'primaryTrait' : 'secondaryTrait',
  value: t.traitId,
  trait: t
}));

export const ACTIVE_FILTER = {
  key: 'isActive',
  value: true
};

export const PASSIVE_FILTER = {
  key: 'isActive',
  value: false
};
