import { Compare } from '@constants/compare';

/*
 * compare two strings lexographically, ignoring case
 */
export function compareStringsCaseInsensitive(string1: string, string2: string): number {
  return string1.toUpperCase().localeCompare(string2.toUpperCase());
}

/*
 * boolean comparator, false values are less than true values
 */
export function compareBooleans(bool1: boolean, bool2: boolean): number {
  if (bool1 === bool2) {
    return Compare.EqualTo;
  }

  return bool1 ? Compare.GreaterThan : Compare.LessThan;
}

/*
 * Returns a comparator that compares values using the given list of functions.
 * The comparator will return the result of the first function in the list that doesn't return 0,
 * or 0 if they all return 0
 */
export function buildComparator<T>(fs: ((v1: T, v2: T) => number)[]): (value1: T, value2: T) => number {
  return (value1: T, value2: T) => {
    for (const f of fs) {
      const result = f(value1, value2);

      if (result !== Compare.EqualTo) {
        return result;
      }
    }

    return Compare.EqualTo;
  };
}
