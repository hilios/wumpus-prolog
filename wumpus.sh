#!/bin/bash
swipl -f $1 -g src/world.pl -s src/main.pl -g "run(random)."
