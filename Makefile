default: compile

base_dir = $(abspath .)
src_dir = $(base_dir)/src/main
gen_dir = $(base_dir)/gen

PROJECT_NAME = iFuCore

SBT = sbt

compile: $(gen_dir)/$(PROJECT_NAME).sv

$(gen_dir)/$(PROJECT_NAME).sv: $(shell find $(src_dir) -name '*.scala')
	$(SBT) "run $(gen_dir)"

log:
	make > ./log

clean:
	rm -rf $(gen_dir)/$(PROJECT_NAME).*

.PHONY: clean log
