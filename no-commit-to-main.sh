#!/bin/sh
if [ "${CI:=false}" == "true" ]; then
    echo "Running on CI. Nothing to check."
    exit 0;
fi

BRANCH=$(git branch --show-current)
if [ "$BRANCH" == "main" ]; then
    echo "No commits allowed on $BRANCH branch!"
    exit 1;
fi
