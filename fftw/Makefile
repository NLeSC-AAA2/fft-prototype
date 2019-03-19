## ------ language="Make" file="Makefile"
build_dir=build
compile=g++
link=g++

codelet_dir=build
codelet_name=fft
codelet_cflags=-O3 -fpic -include codelet.hh
codelet_ldflags=-shared

.PHONY: clean all

## prevent intermediate files from being deleted
.SECONDARY:

all:	$(build_dir)/twiddle-7.gen.so \
	$(build_dir)/notw-7.gen.so \
	$(build_dir)/notw-21.gen.so

$(codelet_dir)/%.gen.cc : Makefile strip-boiler-plate.awk
	@mkdir -p $(@D)
	variant=$(word 1, $(subst -, ,$*)); \
	radix=$(word 2, $(subst -, ,$*)); \
	../genfft/gen_$${variant}.native -n $${radix} -name $(codelet_name) \
	    -standalone -compact -rader-min 17 -circular-min 3 \
	| awk -f strip-boiler-plate.awk \
	| indent -nut > $@

$(build_dir)/%.gen.o : $(codelet_dir)/%.gen.cc codelet.hh
	@mkdir -p $(@D)
	$(compile) $(codelet_cflags) -c $< -o $@

$(build_dir)/%.gen.so : $(build_dir)/%.gen.o
	@mkdir -p $(@D)
	$(link) $^ $(codelet_ldflags) -o $@

clean:
	-rm -rf $(build_dir)
## ------ end
