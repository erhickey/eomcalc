/**
 * contains contants used throughout the app
 */
import {arrayToMap, objectToMapByValues} from '../util/util.js';

import {rarities, skillTypes, traitTypes} from '../../data/enums.json';
import {skills} from '../../data/skills.json';
import {traits} from '../../data/traits.json';

export const IMAGES_DIR = 'assets/img/';
export const SKILL_IMAGES_DIR = IMAGES_DIR + 'skills/';
export const TRAIT_IMAGES_DIR = IMAGES_DIR + 'traits/';

export const SKILL_ID_PREFIX = 'skill-';

export const SKILL_ADDED = 1;
export const SKILL_REMOVED = 2;
export const NO_CHANGE = 3;

export const RARITY_MAP = objectToMapByValues(rarities);
export const SKILLS = skills;
export const SKILL_TYPES = skillTypes;
export const TRAITS = traits;
export const TRAIT_MAP = arrayToMap(traits, 'id');
export const TRAIT_TYPES = traitTypes;
