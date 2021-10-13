/*
 * append a trailing '/' character to a string if it does not already exist
 */
export function addPathSeparator(dir: string): string {
  return dir.endsWith('/') ? dir : dir + '/';
}
