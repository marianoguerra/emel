cd ../ebin
IFS=$'\n';for line in $(cat ../examples/examples.txt);do fnc -r emel rst $(printf "%q" $line); done
