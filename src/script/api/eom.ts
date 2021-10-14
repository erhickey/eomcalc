/*
 * game data
 */

import { rarities } from '@data/rarities.json';
import { skills } from '@data/skills.json';
import { traits } from '@data/traits.json';

import { TRAIT_ID_SECONDARY_BREAKPOINT } from '@constants/eom';
import { createSkills, Skill } from '@typez/skill';
import { createTraits, Trait } from '@typez/trait';

const RARITIES = new Map<number, string>(rarities.map(kv => [kv.key, kv.value]));
export const TRAITS: Trait[] = createTraits(traits, TRAIT_ID_SECONDARY_BREAKPOINT);
export const SKILLS: Skill[] = createSkills(skills, TRAITS, RARITIES);
