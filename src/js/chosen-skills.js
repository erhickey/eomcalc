import {ACTIVE, SKILL_IMAGES_DIR} from './constants.js';
import {createImageNode, parseBuild, replaceChildren, sortSkills, stringifyBuild} from './util.js';

var chosenSkills = [];

function renderChosenSkills(skills) {
  replaceChildren(document.getElementById('chosen-skills'), createChosenSkillComponents(skills));
}

export function chooseSkill(skill) {
  if (chosenSkills.find(s => s.id === skill.id)) {
    removeChosenSkill(skill);
    return;
  }

  if (
    chosenSkills.length === 10 ||
    (skill.type === ACTIVE && chosenSkills.filter(s => s.type === ACTIVE).length === 5)
  ) {
    return;
  }

  chosenSkills.push(skill);
  renderChosenSkills(chosenSkills);
}

export function initChosenSkillsComponent(input) {
  const build = parseBuild(input);

  if (build) {
    chosenSkills = build;
    return createChosenSkillComponents(chosenSkills);
  }

  return createChosenSkillComponents([]);
}

function createChosenSkillComponents(skills) {
  if (skills.length > 0) {
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
    return [info];
  }
}

function createChosenSkillComponent(skill) {
  const component = document.createElement('div');
  component.classList.add('chosen-skill', skill.rarity.toLowerCase());

  const skillImage = createImageNode(SKILL_IMAGES_DIR, skill.name);

  component.appendChild(skillImage);

  component.onclick = function() {
    removeChosenSkill(skill);
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

function removeChosenSkill(skill) {
  chosenSkills = chosenSkills.filter(s => s.id !== skill.id);
  renderChosenSkills(chosenSkills);
}
