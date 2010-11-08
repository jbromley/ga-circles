#!/bin/sh
sbcl --noinform --eval "(asdf:operate 'asdf:load-op :ga-circles-gui)" --eval "(ga-circles-gui:run)" --eval "(sb-ext:quit)"
exit 0
