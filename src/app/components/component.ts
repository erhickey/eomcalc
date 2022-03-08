export abstract class Component {
  private div = document.createElement('div');

  get element(): HTMLDivElement {
    return this.div;
  }

  get classList(): DOMTokenList {
    return this.div.classList;
  }

  appendChild(node: Node): void {
    this.div.appendChild(node);
  }

  get id(): string {
    return this.div.id;
  }

  set id(value: string) {
    this.div.id = value;
  }

  set onclick(value: () => void) {
    this.div.onclick = value;
  }

  replaceChildren(...value: (string | Node)[]): void {
    this.div.replaceChildren(...value);
  }
}
