# The ONLY makefile for the tile-based ASF tools prototype.

# You can override compiler flags "OPTS" from the command line.
#  These arguments get passed straight to the "compile" script.
OPTS=-g

CLUI_SRC=src/asf/clui_cli.cpp
CLUI=bin/clui

CORE_SRC=src/asf/plugin.cpp src/asf/plugin_loader.cpp src/asf/plugin_execute.cpp \
	src/asf/image.cpp src/asf/clui.cpp src/asf/ddr.cpp src/asf/util.cpp \
	src/asf/meta.cpp src/asf/meta_earth.cpp src/asf/meta_sar.cpp \
	src/pup/pup.cpp \
	src/osl/dll.cpp src/osl/dir.cpp
CORE=lib/asf_core.dll

all: test

test: test_plugins test_standalone
	@echo "All tests completed successfully!"

test_standalone: clui all_standalone
	cd tests; ./test_all.sh

test_plugins: clui all_plugins
	cd plugins; ./test_all.sh

dll: $(CORE)

core: $(CORE)

$(CORE): $(CORE_SRC)
	build/compile -Isrc $(CORE_SRC) -o $@ $(OPTS) \
		-DASF_BUILD_COREDLL=1 -DASF_COREDLL_TOPDIR='"'`pwd`'"'

clui: $(CLUI)

$(CLUI): $(CORE) $(CLUI_SRC)
	build/compile -Isrc $(CLUI_SRC) $(CORE) -o $@ $(OPTS) 

all_plugins: $(CORE) plugins/*.cpp
	cd plugins; ./build_all.sh $(OPTS)

all_standalone: $(CORE) src/*/test_*.cpp
	cd tests; ./build_all.sh $(OPTS)

docs:
	doxygen doc/doxygen.cfg

clean:
	-rm $(CORE) $(CLUI) */*.exe */*.dll */*.lib */*.o so_locations plugins/so_locations
	-cd plugins; make clean
