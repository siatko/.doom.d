#!/usr/bin/env bash
set -euo pipefail

for COMMAND in "feh" "wget"; do
    command -v "${COMMAND}" >/dev/null 2>&1
    if [[ $? -ne 0 ]]; then
        echo "I require ${COMMAND} but it's not installed. Abort."
        exit 1
    fi
done

help()
{
   # Display Help
   echo "Get random wallpaper from usplash by keyword(s), save and set it."
   echo
   echo "Syntax: scriptTemplate [-h|k|d|m]"
   echo "options:"
   echo "h     Print this Help."
   echo "k     Keywords separated by comma without space, e.g.: nature,mountains"
   echo "d     Save the wallpapers in this path"
   echo "m     Maximum number of wallpapers to save"
   echo
}

while getopts "hk:d:m:" arg; do
   case $arg in
      h) help
         exit;;
      k) echo ${OPTARG}
         exit;;
      d) echo ${OPTARG}
         exit;;
      m) echo ${OPTARG}
         exit;;
     \?) # Invalid option
         echo "Error: Invalid option"
         exit;;
   esac
done
