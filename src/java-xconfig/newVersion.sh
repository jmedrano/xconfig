#!/bin/sh

VERSION=$1

if [ -z "$VERSION" ]; then
	echo "Please specify new version"
	exit 1
fi

mvn versions:set -DnewVersion=$VERSION-SNAPSHOT
dch -v $VERSION-1

echo "Don't forget to commit and push the changes"
echo "Make an Automated Release and come back to the release branch to make the debian package with cigen"
