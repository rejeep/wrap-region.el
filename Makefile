ECUKES = $(shell find elpa/ecukes-*/ecukes | tail -1)

all: test

test: ecukes

ecukes:
	carton exec ${ECUKES} features

.PHONY: ecukes test all
