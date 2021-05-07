#!/bin/sh

cd "$(dirname "$0")"
VM=java
OPTIONS='-Xms128M -Xmx10g'
$VM $OPTIONS -jar jams-starter.jar $*

