#!/usr/bin/env bash

set -e

pip install ./requirements.txt

python $@
