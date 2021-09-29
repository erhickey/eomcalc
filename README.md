# Echoes of Magic Skill Calculator

https://erhickey.github.io/eomcalc


## About

A simple static site built with vanilla js/css/html. This project has no runtime dependencies, and development dependencies on esbuild and eslint.

Also present in this project is Haskell code which parses data from multiple game files, aggregates the data, and writes it out as JSON for use in the calculator.

## Data JSON Generation Steps:

1. Locate a file named main.###.com.nabooplanet.magicrystal.obb on the device you have Echoes of Magic installed on. On my device it is located at /Internal shared storage/Android/obb/com.nabooplanet.magicrystal/
2. This file is a zip archive, extract its contents
3. Navigate to assets/AssetBundles
4, Use [UtinyRipper](https://github.com/mafaca/UtinyRipper) to extract assets from lua.unity3d
5. Run generate-data with the location of the Ripped folder (created by UtinyRipper) as the only argument

On successful completion, json files will be generated in src/data
