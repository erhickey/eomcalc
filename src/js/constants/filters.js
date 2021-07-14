/*
 * filters to narrow down list of skills
 */

import {SKILL_TYPES, TRAITS, TRAIT_TYPES} from './data.js';

export const TRAIT_FILTERS = TRAITS.map(t => ({
  key: t.type === TRAIT_TYPES.PRIMARY ? 'primaryTrait' : 'secondaryTrait',
  value: t.id,
  trait: t
}));

export const ACTIVE_FILTER = {
  key: 'skillType',
  value: SKILL_TYPES.ACTIVE
};

export const PASSIVE_FILTER = {
  key: 'skillType',
  value: SKILL_TYPES.PASSIVE
};
