import { Skill } from '@typez/skill';
import { Trait } from '@typez/trait';

export class TraitInfo {
  private _active: boolean;
  private _count: number;
  private _nextBreakpoint: number;
  private _currentBreakpointIndex: number;

  constructor(private _trait: Trait, build: Skill[]) {
    const minBreakpoint = Math.min(..._trait.breakpoints);
    const maxBreakpoint = Math.max(..._trait.breakpoints);

    this._count = build.filter(s => s.primaryTrait.id === _trait.id || s.secondaryTrait.id === _trait.id).length;
    this._active = this._count >= minBreakpoint;

    this._nextBreakpoint = this._active ? Math.min(..._trait.breakpoints.filter(n => n > this._count)) : minBreakpoint;

    // will hit this condition when count is higher than or equal to maxBreakpoint
    if (this._nextBreakpoint === Infinity) {
      this._nextBreakpoint = maxBreakpoint;
    }

    this._currentBreakpointIndex = _trait.breakpoints.indexOf(
      Math.max(..._trait.breakpoints.filter(n => n <= this._count))
    );
  }

  public get trait(): Trait {
    return this._trait;
  }

  public get active(): boolean {
    return this._active;
  }

  public get count(): number {
    return this._count;
  }

  public get nextBreakpoint(): number {
    return this._nextBreakpoint;
  }

  public get currentBreakpointIndex(): number {
    return this._currentBreakpointIndex;
  }
}
