export interface SkillJson {
  skillId: number;
  cooldowns: number[];
  descriptions: string[];
  rarity: number;
  secondaryTrait: number;
  isActive: boolean;
  skillName: string;
  primaryTrait: number;
  skillIcon: string;
}

export interface TraitJson {
  traitId: number;
  traitName: string;
  traitDescription: string;
  traitMods: string[];
  traitBreakpoints: number[];
  traitIcon: string;
}
