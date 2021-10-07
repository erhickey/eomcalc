/**
 * constants related to Echoes of Magic
 */

// eslint-disable-next-line @typescript-eslint/no-magic-numbers
export const LEVELS = [1, 2, 3, 4];

export const MAX_SKILLS = 10;
export const MAX_PASSIVES = 7;
export const MAX_ACTIVES = 5;

export const MIN_SKILL_LEVEL = Math.min(...LEVELS);
export const MAX_SKILL_LEVEL = Math.max(...LEVELS);

// traits with ids greater than or equal to this number are secondary traits
export const TRAIT_ID_SECONDARY_BREAKPOINT = 100;

// array index offset
export const LEVEL_OFFSET = 1;
