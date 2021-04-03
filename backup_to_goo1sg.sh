#!/bin/bash
set -x
rsync -rltv --exclude Outlook* . goo1sg:/media/cloud/Miscellaneous/SGDC5CG8080LWZ/Research/Microsoft\ Office/Power\ Query/M\ Scripts/ --progress --inplace --delete $1
