#!/bin/bash

cd "$HOME"
git config --global user.email "travis@travis-ci.org"
git config --global user.name "travis-ci"
git clone --branch=gh-pages https://${GH_TOKEN}@github.com/hasufell/clac gh-pages || exit 1

# haddock
cd gh-pages || exit 1
echo "Removing old docs."
git rm -rf .
echo "Adding new docs."
cp -rf "$TRAVIS_BUILD_DIR"/dist/doc/html/clac/clac-gtk/* . || exit 1
git add *

if [[ -e ./index.html ]] ; then
	echo "Commiting docs."
	git commit -m "Lastest haddock docs updated

	travis build: $TRAVIS_BUILD_NUMBER
	commit: $TRAVIS_COMMIT
	auto-pushed to gh-pages"
	git push origin gh-pages
	echo "Published docs to gh-pages."
else
	echo "Error docs are empty."
	exit 1
fi

