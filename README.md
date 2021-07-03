# Echoes of Magic Skill Calculator

https://erhickey.github.io/eomcalc


## About

A simple static site built with vanilla js/css/html. This project has no runtime dependencies, and development dependencies on esbuild and eslint.

Also present in this project is Haskell code which parses data from multiple game files, aggregates the data, and writes it out as JSON for use in the calculator.

## Skill JSON generation steps:

1. Locate a file named main.###.com.nabooplanet.magicrystal.obb on the device you have Echoes of Magic installed on
2. This file is a zip archive, extract its contents
3. Navigate to assets/bin/Data
4. Find a file containing two groups of four lines for each skill, each set of lines starting with ["SkillName_###"]= or ["SkillTips_###"]=
5. Once you find the file, copy it to src/data/skill-descriptions
6. Find a file containing attributes for each skill. The attributes include an ID, which matches the numbers found in the previous file, and several other attributes such as Desc, quality, SkillType, IsActive, Buy, Sell, CD
7. Once you find the file, copy it to src/data/skill-details
8. Run the Haskell code to generate src/data/skills-generated.json. A cabal file is included to easily execute the code.
