import {initChosenSkillsComponent} from './chosen-skills.js';
import {createSkillListComponents} from './skill-list.js';
import {replaceChildren} from './util.js';

import {skills} from '../data/skills.json';

onLoad();

function onLoad() {
  renderPage(skills);
}

function renderPage(skills) {
  const wrapperEl = document.createElement('wrapper');
  wrapperEl.id = 'wrapper';

  const skillListEl = document.createElement('div');
  skillListEl.id = 'skill-list';
  replaceChildren(skillListEl, createSkillListComponents(skills));
  wrapperEl.appendChild(skillListEl);

  const chosenSkillsEl = document.createElement('div');
  chosenSkillsEl.id = 'chosen-skills';
  const buildParam = new URLSearchParams(window.location.search).get('b');
  replaceChildren(chosenSkillsEl, initChosenSkillsComponent(buildParam));
  wrapperEl.appendChild(chosenSkillsEl);

  document.body.appendChild(wrapperEl);
}
