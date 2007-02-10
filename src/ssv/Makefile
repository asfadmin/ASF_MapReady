include ../../make_support/system_rules

CFLAGS += $(GSL_CFLAGS) $(PROJ_CFLAGS) -I/home/bkerin/local/include \
          -D_GNU_SOURCE \
          $(shell pkg-config --cflags glib-2.0 gtk+-2.0 gtkglext-1.0 \
                                      gthread-2.0) \
          -Wall -O0 -g3

LIBS := $(LIBDIR)/asf_meta.a \
	$(LIBDIR)/libasf_raster.a \
        $(LIBDIR)/libasf_proj.a \
	$(LIBDIR)/libasf_reporting.a \
	$(LIBDIR)/asf.a \
	$(GLIB_LIBS) \
        $(GSL_LIBS) \
	$(JPEG_LIBS) \
        $(PROJ_LIBS) \
	$(GTK_LIBS) \
	$(shell pkg-config --libs glib-2.0 gtk+-2.0 gtkglext-1.0 gthread-2.0) \
	-lm

INTERFACES := pyramid \
              pyramid_cache \
              viewer_image \
              viewer_window \
              float_blob \
              graph \
              meta_read_wrapper \
	      pix_maps_spec \
              ssv_command_line \
	      space_2d \
              table_file \
	      type_mark \
              ec_lock \
              utilities

HEADERS := $(patsubst %, %.h, $(INTERFACES))

LIBOBJS := $(patsubst %, %.o, $(INTERFACES))

LINK_COMMAND = gcc -Wall $< $(LIBOBJS) $(LIBS) -o $@

$(LIBOBJS): $(HEADERS) Makefile

make_subimage: make_subimage.o $(LIBOBJS)
	$(LINK_COMMAND)

ssv: ssv.o $(LIBOBJS)
	$(LINK_COMMAND)

test: check_for_test_pyramid_stamp

test_mesa_glut_stamp: test_mesa_glut.c
	$(CC) -Wall -O0 -g -I$(HOME)/local/include test_mesa_glut.c -c \
              -o test_mesa_glut.o
	$(CC) -Wall test_mesa_glut.o -L$(HOME)/local/lib -lglut -lGL \
              -o test_mesa_glut
	./test_mesa_glut
	touch test_mesa_glut_stamp

test_ec_lock_stamp: test_ec_lock.o $(LIBOBJS)
	$(CC) -Wall $< $(LIBOBJS) $(LIBS) -o test_ec_lock
	./test_ec_lock
	touch $@

test_table_file_stamp: test_table_file.o $(LIBOBJS)
	$(CC) -Wall $^ $(LIBS) -lrt -o test_table_file
	rm -rf /tmp/dumpage
	mkdir /tmp/dumpage
	cp view_graph.perl /tmp/dumpage
	./test_table_file
	 touch $@

# This target isn't invoked automatically.  It runs a 15 minute stress
# test of the TableFile class.  We depend on the regular test stamp
# since we want the results of this target to be at least as strong,
# and since it uses programmed pauses rather than full speed random
# actions it might be if we did only the paused version.
test_table_file_heavy_stress: test_table_file_stamp \
                              test_table_file.c $(LIBOBJS)
	$(CC) $(CPPFLAGS) -DTEST_TABLE_FILE_LONG_TEST_RUN \
               -DTEST_TABLE_FILE_ENABLE_RANDOM_DELAYS $(CFLAGS) \
	       test_table_file.c -c -o test_table_file.o
	$(CC) -Wall test_table_file.o $(LIBOBJS) $(LIBS) -lrt \
              -o test_table_file
	rm test_table_file.o
	@echo "Performing ~15 minute stress test of TableFile class..."
	./test_table_file
	@echo "Stress test of TableFile class successful."

test_graph_depth_first_search_stamp: test_graph_depth_first_search.o $(LIBOBJS)
	$(CC) -Wall $^ $(LIBS) -o test_graph_depth_first_search
	./test_graph_depth_firzst_search
	touch $@

test_graph_cycle_detection_stamp: test_graph_cycle_detection.o $(LIBOBJS)
	$(CC) -Wall $^ $(LIBS) -o test_graph_cycle_detection
	./test_graph_cycle_detection
	touch $@

check_for_test_pyramid_stamp: run_test_pyramid_stamp
	([ -e test_pyramid_stamp ] \
         && echo "Ok, found up-to-date test_pyramid_stamp.") \
	|| (    echo "" \
	     && echo "No test_pyramid_stamp found." \
             && echo "" \
	     && echo "Probably you need to visually inspect the layer JPEG" \
             && echo "images and make sure they look ok, then perform a" \
             && echo "'touch test_pyramid_stamp' as instructed by the " \
             && echo "'./test_pyramid' test program." \
	     && echo "" \
             && false)

test_pyramid: test_pyramid.o $(LIBOBJS)
	$(CC) -Wall $^ $(LIBS) -o $@

run_test_pyramid_stamp: test_pyramid
	./$<
	touch $@

make_wonky_test_image: make_wonky_test_image.o $(LIBOBJS)
	$(LINK_COMMAND)

plugin_histogram: plugin_histogram.o $(LIBOBJS)
	$(LINK_COMMAND)

clean:
	rm -rf *.o *~ core.* core test_pyramid ssv make_subimage \
               test_ec_lock test_graph_cycle_detection \
               test_graph_depth_first_search test_mesa_glut test_table_file \
               make_wonky_test_image test_*_stamp
