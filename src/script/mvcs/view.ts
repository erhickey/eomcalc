/*
 * the view, or the top level component by another name
 */

import { BuildComponent } from '@components/build';
import { SkillDetailComponent } from '@components/skill-detail';
import { SkillListAndFiltersComponent } from '@components/skill-list-and-filters';
import { TraitDetailComponent } from '@components/trait-detail';
import { Controller } from '@mvcs/controller';
import { Service } from '@mvcs/service';

export function initialize(controller: Controller, service: Service, container: HTMLElement): void {
  const df = new DocumentFragment();
  df.appendChild(new SkillDetailComponent(controller, service).element);
  df.appendChild(new TraitDetailComponent(controller, service).element);
  df.appendChild(new SkillListAndFiltersComponent(controller, service).element);
  df.appendChild(new BuildComponent(controller, service).element);
  container.replaceChildren(df);
}
