#! /bin/bash


./searchTodo

cd ..

cd src
ls *.hs */*.hs */*/*.hs | sed /Assets.hs/d | xargs hlint

echo "Creating real assets"
echo "createAssets False \"Assets\" \"Assets.hs\"" | ghci -fno-warn-tabs Utils/CreateAssets.hs 
git add -f Assets.hs
git commit Assets.hs -m "New Assets"
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


cp .stack-work/install/x86_64-linux/lts-8.15/8.0.2/bin/ALGT2-exe binaries/ALGT2-latest
