#!/bin/sh

D=$(dirname $0)
$D/wrk2il "$@" | $D/il2fit
