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

cabal sandbox init
cabal sandbox add-source modules/algebra-dag
cabal sandbox add-source modules/algebra-sql
cabal sandbox add-source modules/dsh
cabal sandbox add-source modules/dsh-example-queries
cabal sandbox add-source modules/dsh-sql
