import { TRAITS } from '@api/eom';
import { Component } from '@components/component';
import { FILTERS_CONTAINER_ID } from '@constants/html';
import { createTraitImage } from '@helpers/images';
import { Controller } from '@mvcs/controller';
import { Service } from '@mvcs/service';
import { Filter, FlexFilter, SkillTypeFilter, TraitFilter } from '@typez/filter';

export class FiltersComponent extends Component {
  private static readonly TEXT_FILTER_KEY = 'text-filter';

  private static readonly textFilterTimeout = 500;

  private traitFilters: HTMLDivElement[];
  private activeFilter: HTMLDivElement;
  private passiveFilter: HTMLDivElement;
  private textFilter: HTMLInputElement;

  constructor(private controller: Controller, service: Service) {
    super();

    this.traitFilters = TRAITS.map(t => new TraitFilter(t)).map(tf => this.createTraitFilter(tf));
    this.activeFilter = this.createTypeFilter(new SkillTypeFilter(true), '[Active Skills]');
    this.passiveFilter = this.createTypeFilter(new SkillTypeFilter(false), '[Passive Skills]');
    this.textFilter = this.createTextFilter();

    this.render();
    this.initSubscriptions(service);
  }

  private render(): void {
    this.id = FILTERS_CONTAINER_ID;
    this.appendChild(this.createTop());
    this.appendChild(this.createTraitFilters());
    this.appendChild(this.createTypeFilters());
  }

  private initSubscriptions(service: Service): void {
    service.filtersChange.subscribe(filters => {
      this.update(filters);
    });
  }

  private update(filters: Filter[]): void {
    [...this.traitFilters, this.activeFilter, this.passiveFilter].forEach(el => {
      if (filters.some(f => f.key === el.id)) {
        el.classList.add('active-filter');
      } else {
        el.classList.remove('active-filter');
      }
    });

    if (!filters.some(f => f.key === FiltersComponent.TEXT_FILTER_KEY)) {
      this.textFilter.value = '';
    }
  }

  private createTop(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('filters-top');
    el.appendChild(this.createClearFiltersButton());
    el.appendChild(this.textFilter);
    return el;
  }

  private createClearFiltersButton(): HTMLSpanElement {
    const el = document.createElement('span');
    el.classList.add('clear-filter-button');
    el.innerHTML = 'Clear All Filters';
    el.onclick = () => this.controller.onClearFiltersClick();
    return el;
  }

  private createTraitFilters(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('trait-filters');

    this.traitFilters.forEach(tf => {
      el.appendChild(tf);
    });

    return el;
  }

  private createTraitFilter(filter: TraitFilter): HTMLDivElement {
    const el = document.createElement('div');
    el.id = filter.key;
    el.classList.add('trait-filter');
    el.appendChild(createTraitImage(filter.trait));
    el.onclick = () => this.controller.onFilterClick(filter);
    return el;
  }

  private createTypeFilters(): HTMLDivElement {
    const el = document.createElement('div');
    el.classList.add('type-filters');
    el.appendChild(this.activeFilter);
    el.appendChild(this.passiveFilter);
    return el;
  }

  private createTypeFilter(filter: SkillTypeFilter, text: string): HTMLDivElement {
    const el = document.createElement('div');
    el.id = filter.key;
    el.classList.add('type-filter');
    el.innerHTML = text;
    el.onclick = () => this.controller.onFilterClick(filter);
    return el;
  }

  private createTextFilter(): HTMLInputElement {
    const el = document.createElement('input');
    el.setAttribute('type', 'search');
    el.classList.add('text-filter');
    el.placeholder = 'Search';

    let timer = 0;
    el.onkeyup = () => {
      clearTimeout(timer);
      timer = setTimeout(
        () => this.controller.onTextFilterChange(FiltersComponent.createSkillTextFilter(el.value)),
        FiltersComponent.textFilterTimeout
      );
    };

    return el;
  }

  private static createSkillTextFilter(text: string): FlexFilter {
    return new FlexFilter(
      FiltersComponent.TEXT_FILTER_KEY,
      false,
      s =>
        '' === text ||
        s.name.toLowerCase().includes(text.toLowerCase()) ||
        s.descriptions.some(d => d.toLowerCase().includes(text.toLowerCase()))
    );
  }
}
