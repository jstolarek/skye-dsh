#!/bin/bash

# Prompts user to remove existing cabal sandbox
function rmsandbox() {
    if [[ -d .cabal-sandbox ]]; then
        while true; do
            read -p "Remove existing cabal sandbox [y/n]? " yn
            case $yn in
                [Yy]* ) cabal sandbox delete; break;;
                [Nn]* ) return;;
                * ) echo "Please answer y or n.";;
            esac
        done
    fi
}

## SETUP MAIN SANDBOX

# Initialize Cabal sandbox and add modules as source dependencies
rmsandbox
cabal sandbox init
cabal sandbox add-source modules/algebra-dag
cabal sandbox add-source modules/algebra-sql
cabal sandbox add-source modules/dsh
cabal sandbox add-source modules/dsh-example-queries
cabal sandbox add-source modules/dsh-sql

## SETUP DSH SANDBOX

echo "================== Setting up DSH sandbox ==================="
cd modules/dsh

# Initialize Cabal sandbox and add modules as source dependencies
rmsandbox
cabal sandbox init
cabal sandbox add-source ../algebra-dag
