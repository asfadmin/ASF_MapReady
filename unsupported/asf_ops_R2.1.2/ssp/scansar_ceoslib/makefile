# SccsId= @(#)makefile	2.41 3/24/98 

GLOBAL_INCLUDE = /ASF/$(ASFVER)/include/sps
LOCAL_INCLUDE  = ../../include/local

LOCAL_LIB      = ../../lib/$(MACH)/local


MPP_DIRECTIVE = -Pv  -Wp,-H/usr/lpp/ppe.poe/include
MPP_DIRECTIVE2 = -Wp,-H$(LOCAL_INCLUDE) -O3 -qfixed=132

FCOMP          = mpxlf
CCOMP          = mpcc
DEBUG          = -C

# Make the local library
$(LOCAL_LIB)/ssp_ceos.a: wayne_c.o  wayne_f.o find_corners.o
	ar rv ssp_ceos.a wayne_c.o wayne_f.o find_corners.o
	#rm -f *.o 
	mv ssp_ceos.a $(LOCAL_LIB)/ssp_ceos.a

clean: 
	rm -f *.o *.lst

#
wayne_c.o: wayne_c.c
	$(CCOMP)  $(DEBUG) -c -I$(LOCAL_INCLUDE) -I$(GLOBAL_INCLUDE) \
	          wayne_c.c
wayne_f.o:  wayne_f.f
	$(FCOMP) $(DEBUG) -c $(MPP_DIRECTIVE) $(MPP_DIRECTIVE2) \
	          wayne_f.f
find_corners.o: find_corners.c
	cc -c find_corners.c 
#
#
#
#
