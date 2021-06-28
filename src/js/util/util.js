/**
 * contains general purpose utility functions
 */

/*
 * replaces each {} in a string with the argument having
 * the same index in the array supplied
 */
export function format(str, args) {
  var i = 0;

  return str.replace(/{}/g, function () {
    return typeof args[i] != 'undefined' ? args[i++] : '';
  });
};

/*
 * true if the object supplied is iterable, false otherwise
 */
export function isIterable(obj) {
    // checks for null and undefined
    if (obj == null) {
      return false;
    }
    return typeof obj[Symbol.iterator] === 'function';
}

/*
 * append the child or children
 */
export function appendChildren(container, children) {
  if (children == null) {
    return
  }

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
  return arr.reduce(function(map, obj) {
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
  return Object.keys(obj).reduce(function(map, key) {
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
