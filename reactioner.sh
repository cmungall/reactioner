#!/bin/sh
docker run -p 9055:9055 -e PORT=9055 -v $PWD:/work -w /work --rm -ti cmungall/reactioner swipl -G0  -p library=/tools/prolog -l /tools/utf8.pl /tools/bin/reactioner -X .cache -i /data/chebi.owl -i /data/go.owl "$@"

