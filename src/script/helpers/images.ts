/*
 * helper functions for creating image nodes
 */

import { SKILL_IMAGES_DIR, TRAIT_IMAGES_DIR } from '@constants/resources';
import { Skill } from '@typez/skill';
import { Trait } from '@typez/trait';
import { addPathSeparator } from '@util/string';

export function createSkillImage(skill: Skill): HTMLImageElement {
  return createImageElement(SKILL_IMAGES_DIR, skill.name);
}

export function createSkillImageSrc(skill: Skill): string {
  return createImageSrc(SKILL_IMAGES_DIR, skill.name);
}

export function createTraitImage(trait: Trait): HTMLImageElement {
  return createImageElement(TRAIT_IMAGES_DIR, trait.name);
}

export function createTraitImageSrc(trait: Trait): string {
  return createImageSrc(TRAIT_IMAGES_DIR, trait.name);
}

export function createImageElement(imgDir?: string, imgName?: string): HTMLImageElement {
  const el = document.createElement('img');
  el.setAttribute('loading', 'lazy');

  if (imgDir && imgName) {
    el.src = createImageSrc(imgDir, imgName);
  }

  return el;
}

function createImageSrc(imgDir: string, imgName: string): string {
  return addPathSeparator(imgDir) + formatImageName(imgName) + '.webp';
}

/*
 * format string to match image file names
 */
function formatImageName(s: string): string {
  return s.toLowerCase().replace(/ /g, '_');
}
