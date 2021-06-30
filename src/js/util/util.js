/**
 * contains general purpose utility functions
 */

/*
 * replaces each {} in a string with the argument having
 * the same index in the array supplied
 */
export function format(str, args) {
  let i = 0;

  return str.replace(/{}/g, () => {
    return 'undefined' == typeof args[i] ? '' : args[i++];
  });
}

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
 * create a map from an object where each attribute of the object
 * makes up an entry where the value of the attribute is the key,
 * and the key of the attribute is the value
 */
export function objectToMapByValues(obj) {
  return Object.keys(obj).reduce((map, key) => {
    map[obj[key]] = key;
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
 * position hover element relative to parent, offset from top-left by given amount
 * if the element bleeds outside the right or bottom viewport, it will be repositioned
 */
export function positionHoverElement(parentElement, hoverElement, offset) {
  const parentOffset = getAbsolutePosition(parentElement);

  let tipX = parentOffset.x + offset;
  hoverElement.style.left = tipX + 'px';

  let tipY = parentOffset.y + offset;
  hoverElement.style.top = tipY + 'px';

  const hoverRect = hoverElement.getBoundingClientRect();

  if (hoverRect.bottom > window.innerHeight) {
    tipY -= hoverRect.height;
  }

  if (hoverRect.right > window.innerWidth) {
    tipX -= hoverRect.width;
  }

  hoverElement.style.left = tipX + 'px';
  hoverElement.style.top = tipY + 'px';
}

/*
 * returns the absolute position (top-left x and y) of an element
 */
export function getAbsolutePosition(el) {
  const rect = el.getBoundingClientRect();

  return {
    x: rect.left + window.scrollX
    , y: rect.top + window.scrollY
  };
}
