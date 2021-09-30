/*
 * application data constants
 */

import {arrayToMap} from '../util/util.js';

import {rarities} from '../../data/rarities.json';
import {skills} from '../../data/skills.json';
import {traits} from '../../data/traits.json';

// eslint-disable-next-line no-magic-numbers
export const LEVELS = [1, 2, 3, 4];

export const RARITIES = rarities;
export const SKILLS = skills;
export const TRAITS = traits;
export const TRAIT_MAP = arrayToMap(traits, 'traitId');
