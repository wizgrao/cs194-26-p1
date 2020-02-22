#!/bin/sh
for i in *.jpg; do stack exec cs194-exe  "${i}" "../out/${i}" > $i.out; done
mv *.out ../out

