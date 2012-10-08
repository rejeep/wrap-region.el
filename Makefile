all:
	@./.cartons/ecukes-*/ecukes features

test:
	@./.cartons/ecukes-*/ecukes features/{${FEATURES}}.feature
