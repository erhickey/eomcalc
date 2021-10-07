# Echoes of Magic Skill Calculator

https://erhickey.github.io/eomcalc


## About

A simple static site built with typescript/css/html. This project has no runtime dependencies, and minimal development dependencies (typescript, esbuild, eslint, @typescript-eslint, prettier).

Included in this project is a Haskell program which parses data from multiple game files, aggregates the data, and writes it out as JSON for use in the calculator.

## Data JSON Generation Steps:

1. Locate a file named main.###.com.nabooplanet.magicrystal.obb on the device you have Echoes of Magic installed on. On my device it is located at /Internal shared storage/Android/obb/com.nabooplanet.magicrystal/
2. This file is a zip archive, extract its contents
3. Navigate to assets/AssetBundles
4. Use [UtinyRipper](https://github.com/mafaca/UtinyRipper) to extract assets from lua.unity3d. A Ripped directory will be created containing the extracted files
5. Run generate-data with the location of the Ripped directory as the only argument

On successful completion, json files will be generated in src/data

## Image Conversion:

Images have been graciously supplied by AsymmetryViolet on [Discord](https://discord.gg/4jSaCgbRyq).

The images supplied are in png format, I have used [ImageMagick](https://imagemagick.org/) to convert and resize them:

    mogrify -resize NxN -format webp *png

Skills have been resized to 50x50, traits to 30x30, and cards have not been resized.

The following command will rename the files appropriately:

    for f in *webp ; do mv "$f" "$(printf "$f" | tr [:upper:] [:lower:] | tr ' ' _)" ; done
