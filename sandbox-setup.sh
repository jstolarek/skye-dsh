#!/bin/bash

if [[ -d .cabal-sandbox ]]; then
    while true; do
        read -p "Remove existing cabal sandbox [y/n]? " yn
        case $yn in
            [Yy]* ) cabal sandbox delete; break;;
            [Nn]* ) exit;;
            * ) echo "Please answer y or n.";;
        esac
    done
fi

# Initialize Cabal sandbox
cabal sandbox init

# Add modules as source dependencies to the sandbox.
cabal sandbox add-source modules/algebra-dag
cabal sandbox add-source modules/algebra-sql
cabal sandbox add-source modules/dsh
cabal sandbox add-source modules/dsh-example-queries
cabal sandbox add-source modules/dsh-sql

# Copy sandbox settings into each submodule directory.  This way it will be
# possible to run `cabal build` within a subdirectory and re-use the master
# sandbox for the project.
cp cabal.sandbox.config modules/algebra-dag
cp cabal.sandbox.config modules/algebra-sql
cp cabal.sandbox.config modules/dsh
cp cabal.sandbox.config modules/dsh-example-queries
cp cabal.sandbox.config modules/dsh-sql
