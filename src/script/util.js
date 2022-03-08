// eslint-disable-next-line @typescript-eslint/explicit-function-return-type
function groupBy(xs, key) {
  return Object.values(
    xs.reduce((acc, x) => {
      (acc[x[key]] = acc[x[key]] || []).push(x);
      return acc;
    }, {})
  );
}

exports.groupBy = groupBy; // eslint-disable-line no-undef
