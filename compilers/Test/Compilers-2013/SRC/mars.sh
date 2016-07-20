#!/bin/sh
#
# Run MARS on the command line without printing its copyright message.

# Assume that Mars4_4.jar is in the root directory.  Otherwise, change
# MARS_PATH.
MARS_PATH="$(dirname "$0")/../Mars4_4.jar"

if [ ! -f "$MARS_PATH" ]; then
    echo "error: please put a Mars4_4.jar file in the root directory of the project."
    exit
fi

java -jar "$MARS_PATH" nc "$@"
