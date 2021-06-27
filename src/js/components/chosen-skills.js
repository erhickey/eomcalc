/*
 * contains functions which create the elements indicating which skills have been chosen
 */

import {ACTIVE, SKILL_IMAGES_DIR} from '../constants/constants.js';
import {onChosenSkillClick} from '../mvc/controller.js';
import {createImageNode, sortSkills, stringifyBuild} from '../util/app-util.js';

export function createChosenSkills(skills) {
  if (skills && skills.length > 0) {
    const buildLinkEl = document.createElement('input');
    buildLinkEl.setAttribute('type', 'text');
    buildLinkEl.setAttribute('disabled', 'true');
    buildLinkEl.classList.add('build-link-input');
    buildLinkEl.value = window.location.origin + window.location.pathname + '?b=' + stringifyBuild(skills);

    const toReturn = skills.sort(sortChosenSkills).map(s => createChosenSkillComponent(s));
    toReturn.unshift(buildLinkEl);
    return toReturn;
  } else {
    const info = document.createElement('div');
    info.innerHTML = 'Choose skills from above to create your build.';
    info.innerHTML += ' A link will appear here allowing you to share your build.';
    return info;
  }
}

function createChosenSkillComponent(skill) {
  const component = document.createElement('div');
  component.classList.add('chosen-skill', skill.rarity.toLowerCase());

  const skillImage = createImageNode(SKILL_IMAGES_DIR, skill.name);

  component.appendChild(skillImage);

  component.onclick = function() {
    onChosenSkillClick(skill);
  }

  return component;
}

function sortChosenSkills(skill1, skill2) {
  if (skill1.type === skill2.type) {
    return sortSkills(skill1, skill2);
  }

  if (skill1.type === ACTIVE) {
    return -1;
  }

  return 1;
}
