import { Component } from '@components/component';
import { HIDDEN_CLASS } from '@constants/css';
import { TRAIT_DETAIL_HOVER_OFFSET, TRAIT_DETAIL_ID, TRAIT_ID_PREFIX } from '@constants/html';
import { createImageElement, createTraitImageSrc } from '@helpers/images';
import { Controller } from '@mvcs/controller';
import { Service } from '@mvcs/service';
import { Trait } from '@typez/trait';
import { TraitInfo } from '@typez/trait-info';
import { positionElementRelativeTo } from '@util/html';

export class TraitDetailComponent extends Component {
  private trait: Trait | undefined;

  private traitImage = createImageElement();
  private header = document.createElement('span');
  private description = document.createElement('div');
  private body = TraitDetailComponent.createBody();

  constructor(private controller: Controller, service: Service) {
    super();
    this.render();
    this.initSubscriptions(service);
  }

  private render(): void {
    this.id = TRAIT_DETAIL_ID;
    this.classList.add('trait-detail');
    this.appendChild(this.createCloseButton());
    this.appendChild(this.createHeader());
    this.appendChild(this.body);
  }

  private initSubscriptions(service: Service): void {
    service.traitDetailChange.subscribe(trait => {
      this.trait = trait.trait;
      this.update(trait);
    });

    service.traitDetailVisibilityChange.subscribe(isVisible => {
      if (isVisible === this.classList.contains(HIDDEN_CLASS)) {
        this.classList.toggle(HIDDEN_CLASS);
      }

      if (isVisible) {
        positionElementRelativeTo(
          document.getElementById(TRAIT_ID_PREFIX + this.trait?.id) ?? document.body,
          this.element,
          TRAIT_DETAIL_HOVER_OFFSET
        );
      }
    });
  }

  private update(trait: TraitInfo): void {
    this.traitImage.src = createTraitImageSrc(trait.trait);
    this.header.innerHTML = trait.trait.name;
    this.description.innerHTML = trait.trait.description;
    this.body.replaceChildren(this.description);

    for (let i = 0; i < trait.trait.mods.length; i++) {
      this.body.appendChild(
        TraitDetailComponent.createDetailRow(
          trait.trait.breakpoints[i],
          trait.trait.mods[i],
          i === trait.currentBreakpointIndex
        )
      );
    }
  }

  private createCloseButton(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('trait-detail-close');
    el.innerHTML = 'x';
    el.onclick = () => this.controller.hideTraitDetail();
    return el;
  }

  private createHeader(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('trait-detail-header');
    el.appendChild(this.traitImage);
    el.appendChild(this.header);
    return el;
  }

  private static createBody(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('trait-detail-body');
    return el;
  }

  private static createDetailRow(
    breakpoint: number | undefined,
    effect: string | undefined,
    isActive: boolean
  ): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('trait-detail-breakpoint-row');

    if (isActive) {
      el.classList.add('trait-detail-breakpoint-row-active');
    }

    el.innerHTML = breakpoint + ': ' + effect;
    return el;
  }
}
