/**
 * responsible for responding to user input
 *
 * updates the model as necessary, and calls appropriate functions in view
 */

import { SKILLS } from '@api/eom';
import { ESCAPE_KEY } from '@constants/keys';
import { BuildChangeResult } from '@constants/results';
import { parseBuild } from '@helpers/build-text';
import { applyFilters } from '@helpers/filter';
import { Model } from '@mvc/model';
import * as View from '@mvc/view';
import { Filter } from '@typez/filter';
import { Skill } from '@typez/skill';
import { TraitInfo } from '@typez/trait-info';

const model = new Model();

/*
 * called once when the app starts
 *
 * first argument is the html element the calculator will be inserted into
 * second argument is a user provided build parameter
 */
export function initialize(container: HTMLElement, buildParam: string): void {
  const build = model.initialize(parseBuild(buildParam, SKILLS));
  View.initialRender(container, SKILLS, build);

  // hide detail components when esc is pressed
  document.onkeydown = event => {
    if (ESCAPE_KEY === event.key) {
      View.hideSkillDetail();
      View.hideTraitDetail();
    }
  };
}

/*
 * called when a skill list skill is clicked
 */
export function onSkillClick(skill: Skill): void {
  const result = model.addOrRemoveSkill(skill);

  if (result.result === BuildChangeResult.NoChange) {
    return;
  }

  View.buildChanged(result.build, skill, result.result === BuildChangeResult.SkillRemoved);
}

/*
 * called when skill card detail button is clicked
 */
export function onSkillDetailClick(skill: Skill): void {
  if (model.updateSkillDetail(skill)) {
    View.updateSkillDetail(skill, model.getSkillLevel());
  } else {
    View.toggleSkillDetail();
  }
}

/*
 * called when the skill detail close button is clicked
 */
export function hideSkillDetail(): void {
  View.hideSkillDetail();
}

/*
 * called when a skill detail level is clicked
 */
export function onSkillLevelClick(level: number): void {
  if (model.setSkillLevel(level)) {
    const skill = model.getSkillDetail();

    if (skill) {
      View.updateSkillDetail(skill, model.getSkillLevel());
    }
  }
}

/*
 * called when a trait is clicked
 */
export function onTraitClick(anchorElement: HTMLElement, trait: TraitInfo): void {
  if (model.updateTraitDetail(trait)) {
    View.updateTraitDetail(anchorElement, trait);
  } else {
    View.toggleTraitDetail();
  }
}

/*
 * called when the trait detail close button is clicked
 */
export function hideTraitDetail(): void {
  View.hideTraitDetail();
}

/*
 * called when a filter is clicked
 */
export function filterClicked(filter: Filter): void {
  const filters = model.addOrRemoveFilter(filter);
  View.filterChanged(applyFilters(filters, SKILLS), filters, model.getBuild());
}

/*
 * called when user clicks clear filters button
 */
export function clearFiltersClicked(): void {
  model.clearFilters();
  View.filterChanged(SKILLS, [], model.getBuild());
}
