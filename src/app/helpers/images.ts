/*
 * helper functions for creating image nodes
 */

import { FALLBACK_IMAGE, SKILL_IMAGES_DIR, TRAIT_IMAGES_DIR } from '@constants/resources';
import { Skill } from '@typez/skill';
import { Trait } from '@typez/trait';
import { addPathSeparator } from '@util/string';

export function createSkillImage(skill: Skill): HTMLImageElement {
  return createImageElement(SKILL_IMAGES_DIR, skill.icon);
}

export function createSkillImageSrc(skill: Skill): string {
  return createImageSrc(SKILL_IMAGES_DIR, skill.icon);
}

export function createTraitImage(trait: Trait): HTMLImageElement {
  return createImageElement(TRAIT_IMAGES_DIR, trait.icon);
}

export function createTraitImageSrc(trait: Trait): string {
  return createImageSrc(TRAIT_IMAGES_DIR, trait.icon);
}

export function createImageElement(imgDir?: string, imgName?: string): HTMLImageElement {
  const el = document.createElement('img');
  el.setAttribute('loading', 'lazy');

  if (imgDir && imgName) {
    el.src = createImageSrc(imgDir, imgName);
  }

  el.onerror = () => {
    if (el.src !== FALLBACK_IMAGE) {
      el.src = FALLBACK_IMAGE;
    }
  };

  return el;
}

function createImageSrc(imgDir: string, imgName: string): string {
  return addPathSeparator(imgDir) + imgName + '.webp';
}
