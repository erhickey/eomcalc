import { MIN_SKILL_LEVEL } from '@constants/eom';
import { SkillJson } from '@typez/json';
import { Trait } from '@typez/trait';

export interface Skill {
  id: number;
  name: string;
  isActive: boolean;
  rarity: string;
  rarityNumber: number;
  primaryTrait: Trait;
  secondaryTrait: Trait;
  descriptions: string[];
  cooldowns: number[];
  currentLevel: number;
  icon: string;
}

export function createSkills(skills: SkillJson[], traits: Trait[], rarities: Map<number, string>): Skill[] {
  return skills.map(s => createSkill(s, traits, rarities));
}

function createSkill(skill: SkillJson, traits: Trait[], rarities: Map<number, string>): Skill {
  return {
    id: skill.skillId,
    name: skill.skillName,
    isActive: skill.isActive,
    rarity: rarities.get(skill.rarity)?.toLowerCase(),
    rarityNumber: skill.rarity,
    primaryTrait: traits.find(t => t.id === skill.primaryTrait),
    secondaryTrait: traits.find(t => t.id === skill.secondaryTrait),
    descriptions: skill.descriptions,
    cooldowns: skill.cooldowns,
    currentLevel: MIN_SKILL_LEVEL,
    icon: skill.skillIcon
  } as Skill;
}
