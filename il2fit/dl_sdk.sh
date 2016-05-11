#!/bin/sh

FIT_VSN=16.73.0
FIT_BASE=https://bitbucket.org/a12n/wrked/downloads
FIT_URL=$FIT_BASE/FitSDKRelease_$FIT_VSN.zip

#-----------------------------------------------------------------------

OUT_DIR=$1
if [ -z "$OUT_DIR" ]; then
    echo "Usage: $0 out_dir"
    exit 1
fi

mkdir -p $OUT_DIR || exit 1

TEMP_FILE=$(mktemp)
trap "rm -f $TEMP_FILE" EXIT

curl -L -u a12n $FIT_URL -o $TEMP_FILE || exit 1
unzip $TEMP_FILE -d $OUT_DIR || exit 1
sed -i.orig '/#include <string>/a\
#include <cstring>
' $OUT_DIR/cpp/fit.hpp || exit 1
