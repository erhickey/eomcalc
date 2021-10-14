import { TRAITS } from '@api/eom';
import { BuildTextsComponent } from '@components/build-text';
import { Component } from '@components/component';
import { SkillCardComponent } from '@components/skill-card';
import { TraitComponent } from '@components/trait';
import { BUILD_AND_TRAITS_CONTAINER_ID, BUILD_SKILLS_CONTAINER_ID, TRAITS_CONTAINER_ID } from '@constants/html';
import { compareBuildSkills, compareTraitComponents } from '@helpers/comparators';
import { Controller } from '@mvcs/controller';
import { Service } from '@mvcs/service';
import { Skill } from '@typez/skill';

export class BuildComponent extends Component {
  buildSkillContainer = document.createElement('div');
  traitsContainer = document.createElement('div');
  traitComponents: TraitComponent[];

  constructor(private controller: Controller, private service: Service) {
    super();
    this.traitComponents = TRAITS.map(t => new TraitComponent(t, this.controller, this.service));
    this.render();
    this.initSubscriptions();
  }

  private render(): void {
    this.id = BUILD_AND_TRAITS_CONTAINER_ID;
    this.appendChild(this.createBuildSkillsContainer());
    this.appendChild(this.createTraitsContainer());
    this.appendChild(new BuildTextsComponent(this.service).element);
  }

  private initSubscriptions(): void {
    this.service.buildChange.subscribe(b => this.update(b));
  }

  private update(build: Skill[]): void {
    if (build.length) {
      this.buildSkillContainer.classList.remove('build-skills-text');
      this.buildSkillContainer.replaceChildren(this.createSkillCards(build.sort(compareBuildSkills)));
    } else {
      this.addBuildSkillsContainerText();
    }

    this.sortTraits();
  }

  private createBuildSkillsContainer(): HTMLDivElement {
    this.buildSkillContainer.id = BUILD_SKILLS_CONTAINER_ID;
    this.addBuildSkillsContainerText();
    return this.buildSkillContainer;
  }

  private addBuildSkillsContainerText(): void {
    this.buildSkillContainer.classList.add('build-skills-text');
    this.buildSkillContainer.innerHTML = 'Choose skills from above to create your build.';
    this.buildSkillContainer.innerHTML += '\nShare your build with the link or discord message below.';
  }

  private createSkillCards(build: Skill[]): DocumentFragment {
    const df = new DocumentFragment();
    build.forEach(s => df.appendChild(new SkillCardComponent(s, this.controller, this.service, true).element));
    return df;
  }

  private createTraitsContainer(): HTMLDivElement {
    this.traitsContainer.id = TRAITS_CONTAINER_ID;
    return this.traitsContainer;
  }

  private sortTraits(): void {
    this.traitsContainer.replaceChildren();
    this.traitComponents.sort(compareTraitComponents).forEach(tc => this.traitsContainer.appendChild(tc.element));
  }
}
