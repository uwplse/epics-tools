#!/bin/bash

cd "$(dirname "$0")/.." # source root
VERSION=0.5
NAME="epics-symbolic-interpreter-$VERSION"
DIR="$(mktemp -d /tmp/XXXXXX)/$NAME"

mkdir "$DIR"
cp -R build/* "$DIR"
ln -f doc/symbolic-interpreter.pdf "$DIR/README.pdf"
ln -f symbolic-evaluator/properties.rkt "$DIR/examples.rkt"

tar cvfz "$NAME.tar.gz" -C "$DIR/.." "$NAME"
echo "shasum '$NAME.tar.gz' = $(shasum "$NAME.tar.gz" | awk '{print $1}')"
