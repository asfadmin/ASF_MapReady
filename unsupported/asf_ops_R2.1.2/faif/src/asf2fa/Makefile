#===============================================================================
# Filename:	Makefile
# Description:	Makefile for FAxmitserver and faif_xmitClient executable
# Creator:	Norbert Piega
# Notes:	
#
# SCCS Info:
#  Makefile
#  1
#  04 Oct 1996
#  @(#)Makefile	1.1
#===============================================================================

include ../faif_makedefs

IFAIF=-I../. -I../../../include/local -I../../../include/global
ILDCE=-I../. -I../../../include/local/dce
IODL= -I../../../include/local/ODL
IALL=$(IFAIF) $(IENCRYPT) $(IODL) $(IDCE) $(ILDCE) -I.

FAUTILS_LIB=../../../lib/$(MACH)/local/libfautils.a
UTIL_LIB=../../../lib/$(MACH)/local/libutil.a
DIRMON_LIB=../../../lib/$(MACH)/local/libdirmon.a
LLIST_LIB=/ASF/$(ASFVER)/lib/$(MACH)/libllist.a
ADEOSPROC_LIB=../../../lib/$(MACH)/local/libADEOSproc.a
ADEOS_LIB=../../../lib/$(MACH)/local/libadeos.a

DEFS=-D$(MACH)
CFLAGS=$(MACHCFLAGS) $(DEFS)
CFLAGS=-g -Dsparc $(DEFS)

IFACE=sendtoFA
SERVER=../../../bin/$(MACH)/FAxmitserver
CLIENT=../../../bin/$(MACH)/faif_xmitClient
FAXMITLIB=../../lib/$(MACH)/global/libfaxmit.a

all:	new_uuid server client  xmitlib

SSRCS = server.c server_send_data.c server_send.c 
SOBJS = $(MACH)/server.o $(MACH)/server_send_data.o $(MACH)/server_send.o
SOBJS0 = server.o server_send_data.o server_send.o

CSRCS = client_main.c client_send_data.c client_alloc.c
COBJS = $(MACH)/client_main.o $(MACH)/client_send_data.o \
        $(MACH)/client_alloc.o
COBJS0 = client_main.o client_send_data.o client_alloc.o
CLOBJS = $(MACH)/client_send_data.o $(MACH)/client_alloc.o

############################
# Create New sendtoFA file #
############################
new_uuid:
	./new_uuid.csh < sendtoFA.template

#####################
# Build interface   #
#####################

$(MACH)/$(IFACE).h $(MACH)/$(IFACE)_cstub.o $(MACH)/$(IFACE)_sstub.o : $(IFACE).idl $(IFACE).acf
	cd $(MACH); $(DCE_IDL) $(DCE_IDLFLAGS) ../$(IFACE).idl

##################
# Build client   #
##################
client:	$(MACH)/$(IFACE).h $(MACH)/$(IFACE)_cstub.o $(COBJS)
	cd $(MACH); $(CC) -o $(CLIENT) $(COBJS0) $(IFACE)_cstub.o \
	$(STATIC) $(ADEOS_LIB) $(ADEOSPROC_LIB) $(DCE_LIB) \
	$(FAUTILS_LIB) \
	$(UTIL_LIB) $(MACHLFLAGS) $(MACHLLIBS) $(SPARC_LIB)

##################
# Build server   #
##################

server: $(MACH)/$(IFACE).h $(MACH)/$(IFACE)_sstub.o $(SOBJS)
	cd $(MACH); $(CC) -o $(SERVER) $(SOBJS0) \
$(IFACE)_sstub.o \
$(ADEOSPROC_LIB) \
$(ADEOS_LIB) \
$(FAUTILS_LIB) \
$(UTIL_LIB) \
$(DIRMON_LIB) \
$(LLIST_LIB) \
$(ENCRYPT_LIB) \
$(MACHLFLAGS) \
$(MACHLLIBS) \
$(DCE_LIB) \
$(SPARC_LIB)

$(MACH)/%.o:%.c $(MACH)/$(IFACE).h
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c ../$<

$(MACH)/%.o:$(MACH)/%.c
	cd $(MACH);$(CC) $(CFLAGS) $(IALL) -c $(notdir $<)

xmitlib: $(CLOBJS) $(MACH)/$(IFACE)_cstub.o
	$(AR) $(FAXMITLIB) $(CLOBJS) $(MACH)/$(IFACE)_cstub.o
	$(RANLIB) $(FAXMITLIB)

dirs:
	-mkdir $(MACH)

clean:
	rm -f $(MACH)/* $(FAXMITLIB)
	cd $(MACH);rm -f $(SERVER) $(CLIENT)
