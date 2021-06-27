/**
 * contains general purpose utility functions
 */

export function format(str, args) {
  var i = 0;

  return str.replace(/{}/g, function () {
    return typeof args[i] != 'undefined' ? args[i++] : '';
  });
};

export function isIterable(obj) {
    // checks for null and undefined
    if (obj == null) {
      return false;
    }
    return typeof obj[Symbol.iterator] === 'function';
}

export function replaceChildren(container, children) {
  if (children == null) {
    return;
  }

  container.replaceChildren();
  const df = new DocumentFragment();

  if (isIterable(children)) {
    for (let c of children) {
      df.appendChild(c);
    }
  } else {
    df.appendChild(children);
  }

  container.appendChild(df);
}

export function addPathSeparator(dir) {
  return dir.endsWith('/') ? dir : dir + '/';
}
