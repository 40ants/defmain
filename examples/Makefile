all:
	ros run \
	    --eval '(push "../" asdf:*central-registry*)' \
	    --eval '(push "./" asdf:*central-registry*)' \
            --eval '(ql:quickload :defmain-example)' \
	    --eval '(asdf:make :defmain-example)' \
	    --quit
