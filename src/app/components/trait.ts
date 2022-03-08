import { Component } from '@components/component';
import { TRAIT_ID_PREFIX } from '@constants/html';
import { createTraitImage } from '@helpers/images';
import { Controller } from '@mvcs/controller';
import { Service } from '@mvcs/service';
import { Trait } from '@typez/trait';
import { TraitInfo } from '@typez/trait-info';

export class TraitComponent extends Component {
  private description = document.createElement('span');
  private _traitInfo: TraitInfo;

  get traitInfo(): TraitInfo {
    return this._traitInfo;
  }

  constructor(private trait: Trait, private controller: Controller, service: Service) {
    super();
    this._traitInfo = new TraitInfo(this.trait, service.build);
    this.render();
    this.initSubscriptions(service);
  }

  private render(): void {
    this.id = TRAIT_ID_PREFIX + this.trait.id;
    this.classList.add('trait');
    this.classList.add('trait-inactive');
    this.appendChild(createTraitImage(this.trait));
    this.appendChild(this.description);
    this.update();

    this.onclick = () => this.controller.onTraitClick(this.trait);
  }

  private initSubscriptions(service: Service): void {
    service.traitChange(this.trait).subscribe(traitInfo => {
      this._traitInfo = traitInfo;
      this.update();
    });
  }

  private update(): void {
    if (this._traitInfo.active) {
      this.classList.remove('trait-inactive');
    } else {
      this.classList.add('trait-inactive');
    }

    this.setDescription();
  }

  private setDescription(): void {
    this.description.innerHTML = this._traitInfo.count + '/' + this._traitInfo.nextBreakpoint;
  }
}
