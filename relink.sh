#!/bin/bash

# The purpose of this program is to re-link the swarm to a new version.

for i in {1..24} # say how many prototype insects you will want
do
  rm ./pupa$i/pter
  # ln ./pter ./pupa$i/pter
  # chmod +x ./pupa$i/pter

  cd ./pupa$i
  ln ../pter ./pter
  cd ..

done

