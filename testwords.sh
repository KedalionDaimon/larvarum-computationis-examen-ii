# inputsections.txt
# the test went as:
# echo "show me it. now." | tr [:punct:] ' ' | tr ' ' '\n' | egrep -v '^$'

# How long is the history for each insect,
# i.e. how many (non-unique) input words are remembered:
historylength=1000

# Make all swarm input uppercase:
cat sysinput.txt | tr a-z A-Z > tmpsysinput.txt
mv tmpsysinput.txt sysinput.txt

cat sysinput.txt | tr [:digit:] ' ' | tr [:punct:] ' ' | tr ' ' '\n' | egrep -v '^$' > inputsections.txt
# there is no | sort | uniq, as you care about all input

foundallwords="true"
for i in `cat inputsections.txt` # i.e. words
do

  wordread=`grep $i ./swarmhistory.txt` # CHANGE THE HISTORY FILE

  if [ "$wordread" = "" ]
  then
    foundallwords="false"
    break
  fi
done

swarminsistence=`cat swarminsistence.txt | tr -d '\n'`
wordread=`grep $i ./swarmhistory.txt` # CHANGE THE HISTORY FILE

if [ "$swarminsistence" = "DORUN" ]
then
  foundallwords="true"
fi

# echo "foundallwords = $foundallwords"

if [ "$foundallwords" = "true" ]
then
  ./pter
  cat inputsections.txt swarmhistory.txt | head -$historylength > tmphistory.txt
  mv tmphistory.txt swarmhistory.txt
fi

