#!/usr/bin/env bash
set -euo pipefail

# Check for required binaries
for COMMAND in "feh" "wget"; do
    if ! command -v ${COMMAND} &> /dev/null
    then
        echo "This script requires ${COMMAND} to be installed and in your path."
        exit
    fi
done

help()
{
   # Display Help
   echo "Get random wallpaper from usplash by keyword(s), save and set it."
   echo
   echo "Syntax: scriptTemplate [-h|k|d|m]"
   echo "options:"
   echo "h     Print this help."
   echo "k     Keywords separated by comma without space, e.g.: nature,mountains."
   echo "m     Maximum number of wallpapers to save."
   echo "r     Wallpaper resolution WIDTHxHEIGHT, e.g.: 2560x1440"
   echo
}

# Set default values
KEYWORDS=nature
MAX_TO_SAVE=5
RESOLUTION=1920x1080

# Get options
while getopts "hk:d:m:r:" arg; do
   case $arg in
      h) help
         exit;;
      k) KEYWORDS=${OPTARG};;
      m) MAX_TO_SAVE=${OPTARG};;
      r) RESOLUTION=${OPTARG};;
     \?) # Invalid option
         echo "Error: Invalid option"
         exit;;
   esac
done

# Create dir if necessary
mkdir -p ~/.unblash

TIMESTAMP=$(date +%s)
wget https://source.unsplash.com/random/${RESOLUTION}/?${KEYWORDS} -O ~/.unblash/${KEYWORDS/,/_}-${RESOLUTION}-${TIMESTAMP}.jpg
feh --bg-center ~/.unblash/${KEYWORDS/,/_}-${RESOLUTION}-${TIMESTAMP}.jpg
