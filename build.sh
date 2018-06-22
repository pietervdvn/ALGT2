#! /bin/bash


./searchTodo

cd src

echo "Creating real assets"
echo "createAssets False \"Assets\" \"Assets.hs\"" | ghci -fno-warn-tabs Utils/CreateAssets.hs 
cd ..

echo "Stack build"
nice -n 19 stack build
STACKEXIT="$?"
echo "EXIT CODE: $STACKEXIT"


cd src
echo "Recreating dev assets" 
echo "createAssets True \"Assets\" \"Assets.hs\"" | ghci -fno-warn-tabs Utils/CreateAssets.hs 
cd ..

if [[ $STACKEXIT -ne 0 ]]
then
	echo "FAILED: stack did not exit properly. Quitting now"
	exit
fi




cp .stack-work/install/x86_64-linux/lts-8.15/8.0.2/bin/ALGT2-exe ALGT

VERSION=`./ALGT -v | sed "s/, .*$//"`
cp ALGT "binaries/ALGT-$VERSION"
cp ALGT "ALGT-$VERSION"
echo "Moved new build to binaries"

git add "binaries/ALGT-$VERSION"
git add "ALGT-$VERSION"

git commit -m "Build #$VERSION"
git push
