import { TraitComponent } from '@components/trait';
import { Skill } from '@typez/skill';
import { Trait } from '@typez/trait';
import { TraitInfo } from '@typez/trait-info';
import { buildComparator, compareBooleans, compareStringsCaseInsensitive } from '@util/compare';

export const compareSkills = buildComparator([compareSkillsByRarity, compareSkillsByName]);

function compareSkillsByName(skill1: Skill, skill2: Skill): number {
  return compareStringsCaseInsensitive(skill1.name, skill2.name);
}

function compareSkillsByRarity(skill1: Skill, skill2: Skill): number {
  return skill2.rarityNumber - skill1.rarityNumber;
}

export const compareBuildSkills = buildComparator([compareSkillsByActive, compareSkills]);

function compareSkillsByActive(skill1: Skill, skill2: Skill): number {
  return compareBooleans(skill2.isActive, skill1.isActive);
}

export const compareTraits = buildComparator([
  compareTraitsByActive,
  compareTraitsByCount,
  compareTraitsByType,
  compareTraitsByName
]);

function compareTraitsByActive(trait1: TraitInfo, trait2: TraitInfo): number {
  return compareBooleans(trait2.active, trait1.active);
}

function compareTraitsByCount(trait1: TraitInfo, trait2: TraitInfo): number {
  return trait2.count - trait1.count;
}

function compareTraitsByType(trait1: TraitInfo, trait2: TraitInfo): number {
  return compareBaseTraitsByType(trait1.trait, trait2.trait);
}

function compareTraitsByName(trait1: TraitInfo, trait2: TraitInfo): number {
  return compareBaseTraitsByName(trait1.trait, trait2.trait);
}

export function compareTraitComponents(tc1: TraitComponent, tc2: TraitComponent): number {
  return compareTraits(tc1.traitInfo, tc2.traitInfo);
}

export const compareBaseTraits = buildComparator([compareBaseTraitsByType, compareBaseTraitsByName]);

function compareBaseTraitsByType(trait1: Trait, trait2: Trait): number {
  return compareBooleans(trait2.isPrimary, trait1.isPrimary);
}

function compareBaseTraitsByName(trait1: Trait, trait2: Trait): number {
  return compareStringsCaseInsensitive(trait1.name, trait2.name);
}
