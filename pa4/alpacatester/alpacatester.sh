#!/bin/bash
COLOR_LIGHT_RED='\033[1;31m'
COLOR_LIGHT_GREEN='\033[1;32m'
COLOR_NONE='\033[0m'

OS=`uname -s | tr -d '[[:space:]]'`

# only print ascii on OSX... probably looks ugly elsewhere
if [ "$OS" == "Darwin" ]; then
  cat logo.txt
else
  printf "alpaca tester\n"
  COLOR_NONE=''
  COLOR_LIGHT_RED=''
  COLOR_LIGHT_GREEN=''
fi
printf "\tv1.3 by Hannes for CSE130\n"
printf "\n"

printf "=========================================\n\n"
printf "Disclaimer: Just because you pass all tests\n"
printf "here does not guarantee you'll pass the graders' tests!\n\n"
printf "=========================================\n"
printf "\n"


# remove files like .DS_STORE created by OSX automatically
# which throws off our directory diffing later
rm -rf ./nano-solutions/.*
rm -rf ./test-solutions/.*

numTests=`ls ../tests/*.ml | wc -l | tr -d '[[:space:]]'`

printf "Found $numTests *.ml tests in pa4/tests.\n"
printf "Generating reference solutions for $numTests tests...\n"
rm -rf ./test-solutions/*

HD_AND_TL_DEF='let hd = List.hd in let tl = List.tl in'

for file in ../tests/*.ml
do
  printf "\tRunning ocaml interpreter on $file and saving result.\n"
  fileName=`basename $file | sed 's/.\{3\}$//'`
  TEST_CODE=`cat $file`
  printf "${HD_AND_TL_DEF} (${TEST_CODE});;" | ocaml | grep -E '\- \: .+' | awk -F= '{print $2}' | cut -c 2- > ./test-solutions/$fileName.txt
  #OCAML_OUTPUT=`printf "${HD_AND_TL_DEF} (${TEST_CODE});;" | ocaml`
  #printf $OCAML_OUTPUT | grep -E '\- \: .+' | awk -F= '{print $2}' | cut -c 2- > ./test-solutions/$fileName.txt
  #printf "${HD_AND_TL_DEF} (${TEST_CODE});;" | ocaml > ./test-solutions/$fileName.txt

done

printf "Running your nano interpreter on $numTests tests...\n"
rm -rf ./nano-solutions/*
for file in ../tests/*.ml
do
  printf "\tRunning your nano interpreter on $file and saving result.\n"
  fileName=`basename $file | sed 's/.\{3\}$//'`
  ../nanoml.byte $file |  grep -E 'out\: .+' | cut -c 6- | rev | cut -c 2- | rev > ./nano-solutions/$fileName.txt
done



printf "Evaluating your score...\n"
failsList=`diff -q ./nano-solutions ./test-solutions | grep -oE 't[0-9]+.txt' | uniq | grep -oE '[0-9]+'`
numFail=`diff -q ./nano-solutions ./test-solutions | wc -l | tr -d '[[:space:]]'`
numPass=$(($numTests-$numFail))

if [ "$numFail" != 0 ]; then
  while read -r failedTestName; do
      printf "\t${COLOR_LIGHT_RED}POSSIBLY FAILED test ../tests/t$failedTestName.ml - check manually!\n"
  done <<< "$failsList"
  printf $COLOR_NONE
fi

printf "Note: For t4 and t14, nanoml output may differ from ocaml output. See piazza post for details.\n\n"

printf "Passed [$numPass / $numTests] tests.\n"
if [ $numPass == $numTests ]; then
  printf "\n\n\n"
  cat success-logo.txt
  printf "\n\n\t\t${COLOR_LIGHT_GREEN}ALL TESTS PASSED!\n"
else
  printf "${COLOR_LIGHT_RED}BACK TO OCAAAAAAAAML!\n"
fi
