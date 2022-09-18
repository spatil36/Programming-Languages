#!/bin/sh

#sets dir to directory containing this script
dir=`dirname $0`

#use $dir to access programs in this directory
#so that this script can be run from any directory.
if (javac "$dir/Prj1Parser.java") ; then
	
	echo "build done"
else

	echo "no build done"
fi


