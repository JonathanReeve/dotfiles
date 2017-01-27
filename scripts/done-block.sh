#!/bin/bash

SCRIPTS_DIR=/home/jon/Documents/Settings/dotfiles/scripts/

DONETODAY=`$SCRIPTS_DIR/donetoday`
DONETHISWEEK=`$SCRIPTS_DIR/donethisweek`

echo $DONETHISWEEK-$DONETODAY

if [[ $DONETODAY -eq 0 ]]; then
    echo "#FF0000"
else
    echo "#00FF00"
fi
