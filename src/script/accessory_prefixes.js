// eslint-disable-next-line no-undef,@typescript-eslint/no-var-requires
const prefixes = require(process.argv[2]);
// eslint-disable-next-line no-undef,@typescript-eslint/no-var-requires
const translations = require(process.argv[3]);

const list = [
  ...new Set(
    prefixes
      .map(ap => {
        const desc = translations['EquipPrefix_Desc_' + ap.ID];
        const name = translations['EquipPrefix_Name_' + ap.ID];
        return {
          lvl: ap.Level,
          rank: ap.PrefixRank,
          name,
          desc
        };
      })
      // eslint-disable-next-line @typescript-eslint/no-magic-numbers,no-extra-parens,no-nested-ternary
      .sort((e1, e2) => (e1.name === e2.name ? 0 : e1.name > e2.name ? 1 : -1))
      .map(ap => `<tr><td>${ap.lvl}</td><td>${ap.rank}</td><td>${ap.name}</td><td>${ap.desc}</td></tr>`)
  )
].join('');

const style = `
body {
  color: rgb(201, 209, 217);
  background-color: rgb(13, 17, 23);
}

table {
  border-collapse: collapse;
}

th {
  min-width: max-content;
  text-align: left;
}

th,td {
  padding: 2px 5px;
}

td {
  border-top: 1px;
  border-bottom: 0;
  border-left: 0;
  border-right: 0;
  border-style: solid;
  border-color: rgb(48,54,61);
}
`;

const styleString = `<style>${style}</style>`;

// eslint-disable-next-line no-console
console.log(`
  <!DOCTYPE html>
  <html>
    <head>
      <meta charset="UTF-8">
    </head>

    <body>
      ${styleString}
      <table>
        <tr>
          <th>Level</th>
          <th>Prefix Rank</th>
          <th>Prefix Name</th>
          <th>Description</th>
        </tr>
        ${list}
      </table>
    </body>
   </html>
`);
