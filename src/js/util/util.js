/**
 * contains general purpose utility functions
 */

/*
 * true if the object supplied is iterable, false otherwise
 */
export function isIterable(obj) {
  // checks for null and undefined
  if (null == obj) {
    return false;
  }

  return 'function' === typeof obj[Symbol.iterator];
}

/*
 * append the child or children
 */
export function appendChildren(container, children) {
  if (null == children) {
    return;
  }

  const df = new DocumentFragment();

  if (isIterable(children)) {
    for (const c of children) {
      df.appendChild(c);
    }
  } else {
    df.appendChild(children);
  }

  container.appendChild(df);
}

/*
 * remove all children from a node and append the child or children supplied
 */
export function replaceChildren(container, children) {
  container.replaceChildren();
  appendChildren(container, children);
}

/*
 * append a trailing '/' character to a string if it does not already exist
 */
export function addPathSeparator(dir) {
  return dir.endsWith('/') ? dir : dir + '/';
}

/*
 * create a map from an array where each entry element in the array
 * makes up an entry where the value is the object, and the key
 * is the value of the attribute specified
 */
export function arrayToMap(arr, key) {
  return arr.reduce((map, obj) => {
    map[obj[key]] = obj;
    return map;
  }, {});
}

/*
 * copy text of element with given id to the user's clipboard
 */
export function copyInputText(inputId) {
  const input = document.getElementById(inputId);
  navigator.clipboard.writeText(input.value);
}

/*
 * compare two strings lexographically, ignoring case
 */
export function compareStringsCaseInsensitive(string1, string2) {
  return string1.toUpperCase().localeCompare(string2.toUpperCase());
}

/*
 * returns true if object is not an array or has no elements
 */
export function isEmpty(arr) {
  return !arr || !arr.length;
}

/*
 * return the last element of an array
 */
export function last(arr) {
  // eslint-disable-next-line no-magic-numbers
  return arr[arr.length - 1];
}

/**
 * position element relative to another element, offset from top-left by given amount
 * attempts to keep the element in the viewport
 */
export function positionElementRelativeTo(relativeElement, hoverElement, offset) {
  const relativePos = relativeElement.getBoundingClientRect();

  // first attempt to position element down and to the right
  let tipX = relativePos.left + offset;
  hoverElement.style.left = tipX + 'px';
  let tipY = relativePos.top + offset;
  hoverElement.style.top = tipY + 'px';

  let hoverRect = hoverElement.getBoundingClientRect();

  // if the element is outside the bottom of the viewport
  // position is above
  if (hoverRect.bottom > window.innerHeight) {
    tipY -= hoverRect.height;
  }

  // if the element is outside the right of the viewport
  // position it to the left
  if (hoverRect.right > window.innerWidth) {
    tipX -= hoverRect.width;
  }

  hoverElement.style.left = tipX + 'px';
  hoverElement.style.top = tipY + 'px';

  hoverRect = hoverElement.getBoundingClientRect();

  // if the element is outside the top of the viewport
  // move it down by the amount it sits outside
  // eslint-disable-next-line no-magic-numbers
  if (0 > hoverRect.top) {
    tipY -= hoverRect.top;
  }

  // if the element is outside the left of the viewport
  // move it right by the amount it sits outside
  // eslint-disable-next-line no-magic-numbers
  if (0 > hoverRect.left) {
    tipX -= hoverRect.width;
  }

  hoverElement.style.left = tipX + 'px';
  hoverElement.style.top = tipY + 'px';
}
