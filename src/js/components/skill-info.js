/*
 * contains functions which create the elements that
 * make up the clickable component to display skill details
 */

import {onSkillDetailsClick} from '../mvc/controller.js';

/*
 * create clickable skill info component to display skill details
 */
export function createSkillInfoComponent(skill, className) {
  const component = document.createElement('div');
  component.classList.add(className);
  const span = document.createElement('span');
  span.innerHTML = '?';
  component.appendChild(span);

  component.onclick = (e) => {
    e.stopPropagation();
    onSkillDetailsClick(skill);
  };

  return component;
}
