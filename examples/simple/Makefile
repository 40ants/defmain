all:
	ros run \
	    --eval '(push "~/projects/lisp/40ants-doc/" asdf:*central-registry*)' \
	    --eval '(push "~/projects/lisp/defmain/" asdf:*central-registry*)' \
	    --eval '(push "../../" asdf:*central-registry*)' \
	    --eval '(push "./" asdf:*central-registry*)' \
            --eval '(ql:quickload :example)' \
	    --eval '(asdf:make :example)' \
	    --quit
