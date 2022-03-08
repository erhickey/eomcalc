# Echoes of Magic Skill Calculator

https://erhickey.github.io/eomcalc

[![Total alerts](https://img.shields.io/lgtm/alerts/g/erhickey/eomcalc.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/erhickey/eomcalc/alerts/) [![Language grade: JavaScript](https://img.shields.io/lgtm/grade/javascript/g/erhickey/eomcalc.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/erhickey/eomcalc/context:javascript)

## About

A static site built with typescript/css/html. The site has several development dependencies, and a single runtime dependency of rxjs. I have tried to keep the architecture as simple and clean as possible with minimal dependencies.

The data and icons to support this project are mined from the game files, which are transferred from a mobile device with [jmtpfs](https://github.com/JasonFerrara/jmtpfs). The data exists as lua tables in various files that are extracted with [AssetRipper](https://github.com/ds5678/AssetRipper). A simple lua script, along with the [lua-cjson](https://github.com/mpx/lua-cjson) library, is used to convert the data to json files. The json files are then transformed into a usable format with javascript and [Node.js](https://nodejs.org/en/). Lastly, [ImageMagick](https://imagemagick.org/) is used to resize and convert the images. In typical fashion, this is all glued together with shell scripts. If you have all the required dependencies installed, and a mobile device connected to your machine, you should be able to perform all of these steps by running [mine-data.sh](/src/sh/mine-data.sh).
