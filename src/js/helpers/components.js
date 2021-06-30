/**
 * helper functions for creating image nodes
 */

import {addPathSeparator} from '../util/util.js';

/*
 * create standard image node
 */
export function createImageNode(imgDir, imgName) {
  const el = document.createElement('img');
  el.setAttribute('loading', 'lazy');
  el.src = addPathSeparator(imgDir) + formatImageName(imgName) + '.webp';
  return el;
}

/*
 * format image name to match app standard
 */
export function formatImageName(s) {
  return s.toLowerCase().replace(/ /g, '_');
}
