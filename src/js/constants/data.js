/*
 * application data constants
 */

import {arrayToMap, objectToMapByValues} from '../util/util.js';

import {rarities, skillTypes, traitTypes} from '../../data/enums.json';
import {skills} from '../../data/skills-generated.json';
import {traits} from '../../data/traits.json';

// eslint-disable-next-line no-magic-numbers
export const LEVELS = [1, 2, 3, 4];

export const RARITY_MAP = objectToMapByValues(rarities);
export const SKILLS = skills;
export const SKILL_TYPES = skillTypes;
export const TRAITS = traits;
export const TRAIT_MAP = arrayToMap(traits, 'id');
export const TRAIT_TYPES = traitTypes;
