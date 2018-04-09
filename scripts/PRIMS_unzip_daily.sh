#!/bin/bash

# Uncompressing PRISM daily met files for 1981-2017 so we can work with them

prism_base=/home/data/PRISM

vars=(ppt tmax tmin)

for VAR in ${vars[@]}
do
	pushd $prism_base/daily/$VAR
	yrs_proc=(*)
	
	for YR in ${yrs_proc[@]}
	do
		pushd $YR
			days=(*.zip)
			for DAY in ${days[@]}
			do
				unzip $DAY
			done
		popd
	done
	
	popd
done