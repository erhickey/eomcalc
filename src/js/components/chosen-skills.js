/*
 * contains functions which create the elements indicating which skills have been chosen
 */

import {ACTIVE, SKILL_IMAGES_DIR} from '../constants/constants.js';
import {onChosenSkillClick} from '../mvc/controller.js';
import {
  createBuildLink,
  createImageNode,
  sortSkills,
  generateBuildDiscordMsg,
  generateBuildUrlParam
} from '../util/app-util.js';

export function createChosenSkills(skills) {
  if (skills && skills.length > 0) {

    const buildLinkEl = createBuildLink(
      'Link to Build',
      window.location.origin + window.location.pathname + '?b=' + generateBuildUrlParam(skills),
      'build-link-input');

    const discordMsgEl = createBuildLink('Discord Message', generateBuildDiscordMsg(skills), 'discord-msg-input');

    const toReturn = skills.sort(sortChosenSkills).map(s => createChosenSkillComponent(s));
    toReturn.push(buildLinkEl);
    toReturn.push(discordMsgEl);
    return toReturn;
  } else {
    const info = document.createElement('div');
    info.innerHTML = 'Choose skills from above to create your build.';
    info.innerHTML += ' Share your build with the link or discord message that appears here.';
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
