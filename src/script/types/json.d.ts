export interface SkillJson {
  skillId: number;
  cooldowns: number[];
  descriptions: string[];
  rarity: number;
  secondaryTrait: number;
  isActive: boolean;
  skillName: string;
  primaryTrait: number;
}

export interface TraitJson {
  traitId: number;
  traitName: string;
  traitDescription: string;
  traitMods: string[];
  traitBreakpoints: number[];
}
