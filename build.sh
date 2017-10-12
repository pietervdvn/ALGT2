#! /bin/bash


./searchTodo

cd src
ls *.hs */*.hs */*/*.hs | sed /Assets.hs/d | xargs hlint

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



rm ALGT
rm ALGT-*

cp .stack-work/install/x86_64-linux/lts-8.15/8.0.2/bin/ALGT2-exe ALGT

VERSION=`./ALGT -v | sed "s/, .*$//"`
cp ALGT "binaries/ALGT-$VERSION"
cp ALGT "ALGT-$VERSION"
echo "Moved new build to binaries"

