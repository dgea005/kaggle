#!/bin/bash
for f in data/train/*.tif
do
	filename=$(basename "$f")
	extension="${filename##*.}"
	filename="${filename%.*}"
	echo $filename
	convert $f -resize 80 data/train_scaled/$filename.png
done

for f in data/test/*.tif
do
	filename=$(basename "$f")
	extension="${filename##*.}"
	filename="${filename%.*}"
	echo $filename
	convert $f -resize 80 data/test_scaled/$filename.png
done
