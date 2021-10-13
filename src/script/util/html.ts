/*
 * copy value of input element to the user's clipboard
 */
export function copyInputText(inputElement: HTMLInputElement): void {
  navigator.clipboard.writeText(inputElement.value);
}

// represents left most and top most coordinate
const TOP_LEFT = 0;

/**
 * position element relative to another element, offset from top-left by given amount
 * attempts to keep the element in the viewport
 */
export function positionElementRelativeTo(
  relativeElement: HTMLElement,
  hoverElement: HTMLElement,
  offset: number
): void {
  const relativePos = relativeElement.getBoundingClientRect();

  // first attempt to position element down and to the right
  let tipX = relativePos.left + offset;
  hoverElement.style.left = tipX + 'px';
  let tipY = relativePos.top + offset;
  hoverElement.style.top = tipY + 'px';

  let hoverRect = hoverElement.getBoundingClientRect();

  // if the element is outside the bottom of the viewport
  // position above
  if (hoverRect.bottom > window.innerHeight) {
    tipY -= hoverRect.height;
  }

  // if the element is outside the right of the viewport
  // position to the left
  if (hoverRect.right > window.innerWidth) {
    tipX -= hoverRect.width;
  }

  hoverElement.style.left = tipX + 'px';
  hoverElement.style.top = tipY + 'px';

  hoverRect = hoverElement.getBoundingClientRect();

  // if the element is outside the top of the viewport
  // move it down by the amount it sits outside
  if (TOP_LEFT > hoverRect.top) {
    tipY -= hoverRect.top;
  }

  // if the element is outside the left of the viewport
  // move it right by the amount it sits outside
  if (TOP_LEFT > hoverRect.left) {
    tipX -= hoverRect.width;
  }

  hoverElement.style.left = tipX + 'px';
  hoverElement.style.top = tipY + 'px';
}
