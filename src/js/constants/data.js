/*
 * application data constants
 */

import {arrayToMap, objectToMapByValues} from '../util/util.js';

import {rarities, skillTypes, traitTypes} from '../../data/enums.json';
import {skills} from '../../data/skills.json';
import {traits} from '../../data/traits.json';

export const RARITY_MAP = objectToMapByValues(rarities);
export const SKILLS = skills;
export const SKILL_TYPES = skillTypes;
export const TRAITS = traits;
export const TRAIT_MAP = arrayToMap(traits, 'id');
export const TRAIT_TYPES = traitTypes;
