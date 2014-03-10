#!/bin/sh

VERSION=$1

if [ -z '$VERSION' ]; then
	echo "Please specify new version"
	exit 1
fi

mvn versions:set -DnewVersion=$VERSION
dch -v $VERSION-1

echo "Don't forget to commit the changes and tag this version"
echo "git commit -a -m \"Published version $VERSION\""
echo "git tag java-xconfig-$VERSION"
echo "git push origin"

