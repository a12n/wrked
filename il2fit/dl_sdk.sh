#!/bin/sh

FIT_VSN=16.60.0
FIT_BASE=https://www.thisisant.com/assets/resources/FIT
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

curl $FIT_URL -o $TEMP_FILE || exit 1
unzip $TEMP_FILE -d $OUT_DIR || exit 1
sed -i.orig '/#include <string>/a\
#include <cstring>
' $OUT_DIR/cpp/fit.hpp || exit 1
