#!/bin/bash 
# This script must be executed from MATCOM source root directory!
# It is indented to be used by CI Service

# This command prints command executed to terminal
# Also makes script terminate if any command ends with non-0 exit status
set -ex

# Create build folder (Remove if it exists)
# NOTE: MUst have space between " and ]]
# ./Build" ]] -> WORKS
# ./Build"]]  -> 'conditional binary operator expected'
#[[ -d "./Build" ]] && rm -rf ./Build
#mkdir ./Build

# Running on my home server.. have nagging privilege issues 
# prepending with sudo 
sudo cmake -E chdir ./Build cmake ./..
sudo make -C Build 
