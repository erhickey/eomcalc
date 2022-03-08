#!/usr/bin/env bash

set -e

PROJECT_DIR="$(dirname "$0")/../.."
LUA2JSON="$PROJECT_DIR/src/lua/lua2json.lua"
DATA_DEST="$PROJECT_DIR/src/data"
SKILL_IMAGES_DEST="$PROJECT_DIR/src/img/skills"
TRAIT_IMAGES_DEST="$PROJECT_DIR/src/img/traits"

source "$PROJECT_DIR/src/sh/util.sh"

check_exe_exists mtp-files "jmtpfs"
check_exe_exists mtp-getfile "jmtpfs"
check_exe_exists unzip "unzip"
check_exe_exists AssetRipperConsole "AssetRipperConsole"
check_exe_exists lua "Lua and Lua CJSON"
check_exe_exists node "node.js"
check_exe_exists mogrify "ImageMagick"

echo 'Finding game file on device'
file_id="$(mtp-files | grep -B 1 -i naboo | grep 'File ID:' | cut -d ' ' -f 3)"

[ -n "$file_id" ] || exit 2

WORKING_DIR="$(mktemp -d)"

ASSETS_DIR="$WORKING_DIR/assets/AssetBundles"

LUA_EXTRACT_DIR="$WORKING_DIR/lua"
LUA_DIR="$LUA_EXTRACT_DIR/Assets/Asset_Bundles/lua.unity3d/temp/lua/config"

SKILL_IMAGES_EXTRACT_DIR="$WORKING_DIR/skill_images"
SKILL_IMAGES_DIR="$SKILL_IMAGES_EXTRACT_DIR/Assets/Asset_Bundles/ui/skill1.unity3d/gameassets/ui/skill1"
TRAIT_IMAGES_EXTRACT_DIR="$WORKING_DIR/trait_images"
TRAIT_IMAGES_DIR="$TRAIT_IMAGES_EXTRACT_DIR/Assets/Asset_Bundles/ui/jiban.unity3d/gameassets/ui/jiban"

JSON_DIR="$WORKING_DIR/json"
ACC_PREFIXES_JSON="$JSON_DIR/accessory_prefix_config.json"
SKILLS_JSON="$JSON_DIR/skill_config.json"
TRAITS_JSON="$JSON_DIR/skill_trait_config.json"
EN_US_JSON="$JSON_DIR/en_us.json"

printf 'Copying game file to temp directory: %s\n' "$WORKING_DIR"
mtp-getfile "$file_id" "$WORKING_DIR/eom.obb"

echo 'Extracting contents of game file'
unzip -q "$WORKING_DIR/eom.obb" -d "$WORKING_DIR"

echo 'Ripping lua assets'
AssetRipperConsole -q -o "$LUA_EXTRACT_DIR" "$ASSETS_DIR/lua.unity3d"
echo 'Ripping skill image assets'
AssetRipperConsole -q -o "$SKILL_IMAGES_EXTRACT_DIR" "$ASSETS_DIR/ui/skill1.unity3d"
echo 'Ripping trait image assets'
AssetRipperConsole -q -o "$TRAIT_IMAGES_EXTRACT_DIR" "$ASSETS_DIR/ui/jiban.unity3d"

echo 'Converting lua assets to json'
mkdir "$WORKING_DIR/json"
lua "$LUA2JSON" "$LUA_DIR/SkillConfig.lua.txt" "$SKILLS_JSON"
lua "$LUA2JSON" "$LUA_DIR/SkillTraitConfig.lua.txt" "$TRAITS_JSON"
lua "$LUA2JSON" "$LUA_DIR/AccessoryPrefixConfig.lua.txt" "$ACC_PREFIXES_JSON"
lua "$LUA2JSON" "$LUA_DIR/language/en_us.lua.txt" "$EN_US_JSON"

echo 'Creating accessory_prefixes.html'
node "$PROJECT_DIR/src/script/accessory_prefixes.js" "$ACC_PREFIXES_JSON" "$EN_US_JSON" > "$PROJECT_DIR/src/html/accessory_prefixes.html"

rm -f "$SKILL_IMAGES_DEST/"*
rm -f "$TRAIT_IMAGES_DEST/"*
rm -f "$DATA_DEST/"*

echo 'Creating rarities.json'
printf '[{"key":1,"value":"COMMON"},{"key":2,"value":"UNCOMMON"},{"key":3,"value":"RARE"},{"key":4,"value":"EPIC"},{"key":5,"value":"LEGENDARY"}]' > "$DATA_DEST/rarities.json"

echo 'Creating skills.json, traits.json, and copying skill/trait images'
node "$PROJECT_DIR/src/script/compile_data.js" "$SKILLS_JSON" "$TRAITS_JSON" "$EN_US_JSON" "$DATA_DEST" "$SKILL_IMAGES_DIR" "$TRAIT_IMAGES_DIR" "$SKILL_IMAGES_DEST" "$TRAIT_IMAGES_DEST"

echo 'Converting and resizing images'
mogrify -resize 50x50 -format webp "$SKILL_IMAGES_DEST"/*.png
rm -f "$SKILL_IMAGES_DEST"/*.png
mogrify -resize 30x30 -format webp "$TRAIT_IMAGES_DEST"/*.png
rm -f "$TRAIT_IMAGES_DEST"/*.png

echo 'Cleaning up'
rm -rf "$WORKING_DIR"
