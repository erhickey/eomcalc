#!/usr/bin/env bash

if [ "$#" -ne 1 ] ; then
    echo 'illegal number of arguments'
    echo 'one argument indicating the location of the game data files should be passed'
    exit 1
fi

cd "$1/assets/bin/Data" || exit 2

find_file()
{
    found_file="$(find . -maxdepth 1 -type f -print0 | xargs -0 grep -l "$1")"

    if [ -z "$found_file" ] ; then
        printf 'file not found, exiting.\n'
        exit 3
    fi

    printf '%s' "$(pwd)/$found_file"
}

printf 'Searching for skill and trait descriptions file...'
desc_file="$(find_file "^return LanguageConfig")"
printf 'found: %s\n' "$desc_file"

printf 'Searching for skill details file...'
detail_file="$(find_file "^return SkillConfig")"
printf 'found: %s\n' "$detail_file"

cd - || exit 4
rm src/data/*json
printf 'Generating JSON...\n'
cabal new-run generate-data -- "$desc_file" "$detail_file"
