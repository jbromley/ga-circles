#!/bin/sh
circles=50
pop_size=50
draw_mode=":best"

while getopts ":c:p:d:" opt
do
    case $opt in
	c) 
	    circles=$OPTARG
	    ;;
	p) 
	    pop_size=$OPTARG
	    ;;
	d)
	    case $OPTARG in
		best)
		    draw_mode=":best"
		    ;;
		viable)
		    draw_mode=":viable"
		    ;;
		all) 
		    draw_mode=":all"
		    ;;
		*) 
		    echo "Usage: $0 [-c circles] [-p population] [-d best|viable|all]"
		    ;;
	    esac
	    ;;
	-*)
	    echo "Usage: $0 [-c circles] [-p population] [-d best|viable|all]"
	    ;;
    esac
done

sbcl --noinform --eval "(asdf:operate 'asdf:load-op :ga-circles-gui)" \
    --eval "(ga-circles-gui:run :circles $circles :pop-size $pop_size :draw-mode $draw_mode)" \
    --eval "(sb-ext:quit)"
exit 0
