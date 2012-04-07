#!/bin/bash

# start the release

if [[ $# -lt 2 ]]; then
  echo "usage: $(basename $0) previous-version new-version" >&2
  exit 1
fi

previous_version=$1
version=$2

$(dirname $0)/start-release.sh $previous_version $version
$(dirname $0)/finish-release.sh $version
