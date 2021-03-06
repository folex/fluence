#!/bin/bash

# Uses to start Fluence node with Swarm and Ethereum node on Rinkeby chain.

set -e

USAGE="Usage: ./rinkeby-compose.sh <external-ip> <owner-address> <private-key> <api-port> <capacity>"

if [ ! $1 = '--help' -a ! $1 = '-h' ]; then

    if [ $# -eq 4 ]; then

        if [ ! -f contract.txt ]; then
            echo "File contract.txt not found!"
            exit 125
        fi

        export PROD_DEPLOY='true'
        export CHAIN='rinkeby'
        export ETHEREUM_SERVICE='geth'
        export NAME='fluence-node-1'
        export CONTRACT_ADDRESS=$(cat contract.txt)
        export HOST_IP="$1"
        export OWNER_ADDRESS="$2"
        export PRIVATE_KEY="$3"
        export API_PORT="$4"
        export CAPACITY="$5"
        ./compose.sh deploy
    else
        echo "Error: Not enough arguments."
        echo $USAGE
        exit 125
    fi
else
    echo $USAGE
fi
