const prefixes = require('./ap.json');
const translations = require('./en.json');

const list = [...new Set(prefixes
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
  .sort((e1, e2) => e1.name == e2.name ? 0 : (e1.name > e2.name ? 1 : -1))
  .map(ap => `<tr><td>${ap.lvl}</td><td>${ap.rank}</td><td>${ap.name}</td><td>${ap.desc}</td></td>`)
)].join('');

const style = `
body {
  color: navy;
}

table {
  border-collapse: collapse;
}

th,td {
  padding: 2px 5px;
}

th {
  min-width: max-content;
  text-align: left;
}

tr {
  background-color: slategrey;
}

tr:nth-child(even) {
  background-color: lightslategrey;
}
`;

const styleString = `<style>${style}</style>`;

console.log(`<html>${styleString}<table><tr><th>Level</th><th>Prefix Rank</th><th>Prefix Name</th><th>Description</th>${list}</table></html>`);
