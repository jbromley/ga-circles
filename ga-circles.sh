#!/bin/sh
circles=50
pop_size=50

while getopts ":c:p:" opt
do
    case $opt in
	c) 
	    circles=$OPTARG
	    ;;
	p) 
	    pop_size=$OPTARG
	    ;;
	-*)
	    echo "Usage: $0 [-c circles] [-p population]"
	    ;;
    esac
done

sbcl --noinform --eval "(asdf:operate 'asdf:load-op :ga-circles-gui)" \
    --eval "(ga-circles-gui:run :circles $circles :pop-size $pop_size)" \
    --eval "(sb-ext:quit)"
exit 0
