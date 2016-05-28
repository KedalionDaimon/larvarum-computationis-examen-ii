#!/bin/bash

# The purpose of this program is to initialise a new swarm.
# It also determines the size of the swarm.

rm insects.txt 2>/dev/null
rm -r pupa* 2>/dev/null
# redirect errors to /dev/null

for i in {1..24} # say how many prototype insects you will want
do
  echo $i >> insects.txt
  mkdir ./pupa$i # "insect$i.txt"
  cp ./sysarray.txt ./syswords.txt ./swarminsistence.txt ./sysinput.txt ./sysanswer.txt ./testwords.sh ./swarmhistory.txt ./pupa$i

  # cp ./pter ./pupa$i/pter
  # ln ./pter ./pupa$i/pter
  cd ./pupa$i
  ln ../pter ./pter
  cd ..

  chmod +x ./pupa$i/pter
done

