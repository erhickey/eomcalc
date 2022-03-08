/* eslint-disable no-undef,@typescript-eslint/no-var-requires */

const { groupBy } = require('./util');

const skillsJson = require(process.argv[2]);
const traitsJson = require(process.argv[3]);
const enUSJson = require(process.argv[4]);

const dataDest = process.argv[5];
const skillImagesSrc = process.argv[6];
const traitImagesSrc = process.argv[7];
const skillImagesDest = process.argv[8];
const traitImagesDest = process.argv[9];

const SKILL_LEVELS = 4;
const JSON_INDENTATION = 2;

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type
function iconName(i) {
  return i.substring(i.lastIndexOf('/') + 1); // eslint-disable-line @typescript-eslint/no-magic-numbers
}

const skills = groupBy(
  skillsJson.map(s => ({
    id: s.ID,
    name: enUSJson['SkillName_' + String(s.ID)],
    desc: enUSJson['SkillTips_' + String(s.ID)],
    rarity: s.Quality,
    isActive: Boolean(s.IsActive),
    cooldown: s.CD,
    primaryTrait: s.CareerTrait,
    secondaryTrait: s.ElementTrait,
    icon: s.Icon
  })),
  'name'
)
  .filter(ss => ss.length === SKILL_LEVELS)
  .map(ss => {
    const sorted = ss.sort((s1, s2) => s1.id - s2.id);
    return {
      skillId: sorted[0].id,
      skillName: sorted[0].name,
      rarity: sorted[0].rarity,
      isActive: sorted[0].isActive,
      primaryTrait: sorted[0].primaryTrait[0],
      secondaryTrait: sorted[0].secondaryTrait[0],
      skillIcon: iconName(sorted[0].icon),
      cooldowns: sorted.map(s => s.cooldown),
      descriptions: sorted.map(s => s.desc)
    };
  })
  .sort((s1, s2) => s1.skillId - s2.skillId)
  .filter(s => Boolean(s.primaryTrait));

console.log(`Found ${skills.length} skills.`); // eslint-disable-line no-console

const traits = traitsJson
  .map(t => ({
    traitId: t.ID,
    traitName: enUSJson['TraitName_' + String(t.ID)],
    traitDescription: enUSJson['TraitTip_' + String(t.ID)],
    traitIcon: iconName(t.Icon),
    traitBreakpoints: t.TraitBuffs.map(b => b.num),
    traitMods: enUSJson['TraitTips_' + String(t.ID)].split('|')
  }))
  .sort((t1, t2) => t1.id - t2.id);

console.log(`Found ${traits.length} traits.`); // eslint-disable-line no-console

const fs = require('fs');

fs.writeFileSync(`${dataDest}/skills.json`, JSON.stringify(skills, null, JSON_INDENTATION));
fs.writeFileSync(`${dataDest}/traits.json`, JSON.stringify(traits, null, JSON_INDENTATION));

const skillImages = skills.map(s => s.skillIcon);
const traitImages = traits.map(t => t.traitIcon);

skillImages.forEach(s => {
  fs.copyFile(`${skillImagesSrc}/${s}.png`, `${skillImagesDest}/${s}.png`, err => {
    if (err) {
      console.log(`Error copying skill icon: ${s}`); // eslint-disable-line no-console
      console.log(err); // eslint-disable-line no-console
    }
  });
});

traitImages.forEach(t => {
  fs.copyFile(`${traitImagesSrc}/${t}.png`, `${traitImagesDest}/${t}.png`, err => {
    if (err) {
      console.log(`Error copying trait icon: ${t}`); // eslint-disable-line no-console
      console.log(err); // eslint-disable-line no-console
    }
  });
});
