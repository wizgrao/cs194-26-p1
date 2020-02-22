#!/bin/sh
echo "| Original | Colored | Offset |"
echo "| --- | --- | --- |"
for i in *.jpg; 
do
  b=$( cat "../out/${i}.out"  | tr -d '\n'); 
  echo "| ![yuh](in/${i}) | ![yeet](../out/${i}) | $b |" ; 
done

