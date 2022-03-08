import { TraitJson } from '@typez/json';

export interface Trait {
  id: number;
  name: string;
  description: string;
  isPrimary: boolean;
  mods: string[];
  breakpoints: number[];
  icon: string;
}

export function createTraits(traits: TraitJson[], traitIdSecondaryBreakpoint: number): Trait[] {
  return traits.map(t => createTrait(t, traitIdSecondaryBreakpoint));
}

function createTrait(trait: TraitJson, traitIdSecondaryBreakpoint: number): Trait {
  return {
    id: trait.traitId,
    name: trait.traitName,
    description: trait.traitDescription,
    isPrimary: traitIdSecondaryBreakpoint > trait.traitId,
    mods: trait.traitMods,
    breakpoints: trait.traitBreakpoints,
    icon: trait.traitIcon
  };
}
