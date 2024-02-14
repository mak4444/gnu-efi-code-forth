#
#  Copyright (C) 1999-2001 Hewlett-Packard Co.
#	Contributed by David Mosberger <davidm@hpl.hp.com>
#	Contributed by Stephane Eranian <eranian@hpl.hp.com>
#
#    All rights reserved.
#
#    Redistribution and use in source and binary forms, with or without
#    modification, are permitted provided that the following conditions
#    are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    * Redistributions in binary form must reproduce the above
#      copyright notice, this list of conditions and the following
#      disclaimer in the documentation and/or other materials
#      provided with the distribution.
#    * Neither the name of Hewlett-Packard Co. nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
#    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
#    CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
#    INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
#    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
#    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
#    BE LIABLE FOR ANYDIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
#    OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#    PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
#    TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
#    THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
#    SUCH DAMAGE.
#

SRCDIR = .

VPATH = $(SRCDIR)

include $(SRCDIR)/../Make.defaults

TOPDIR = $(SRCDIR)/..

CDIR=$(TOPDIR)/..
LINUX_HEADERS	= /usr/src/sys/build
APPSDIR		= $(LIBDIR)/gnuefi/apps
CPPFLAGS	+= -D__KERNEL__ -I$(LINUX_HEADERS)/include
CRTOBJS		= ../gnuefi/crt0-efi-$(ARCH).o

LDSCRIPT	= $(TOPDIR)/gnuefi/elf_$(ARCH)_efi.lds
ifneq (,$(findstring FreeBSD,$(OS)))
LDSCRIPT	= $(TOPDIR)/gnuefi/elf_$(ARCH)_fbsd_efi.lds
endif

LDFLAGS		+= -shared -Bsymbolic -L../lib -L../gnuefi $(CRTOBJS)

LOADLIBES	+= -lefi -lgnuefi
LOADLIBES	+= $(LIBGCC)
LOADLIBES	+= -T $(LDSCRIPT)

TARGET_APPS = forth.efi
TARGET_BSDRIVERS =
TARGET_RTDRIVERS =

ifneq ($(HAVE_EFI_OBJCOPY),)

FORMAT		:= --target efi-app-$(ARCH)
$(TARGET_BSDRIVERS): FORMAT=--target efi-bsdrv-$(ARCH)
$(TARGET_RTDRIVERS): FORMAT=--target efi-rtdrv-$(ARCH)

else

SUBSYSTEM	:= 0xa
$(TARGET_BSDRIVERS): SUBSYSTEM = 0xb
$(TARGET_RTDRIVERS): SUBSYSTEM = 0xc

FORMAT		:= -O binary
LDFLAGS		+= --defsym=EFI_SUBSYSTEM=$(SUBSYSTEM)

endif

TARGETS = $(TARGET_APPS) $(TARGET_BSDRIVERS) $(TARGET_RTDRIVERS)


FORTH_SRC = Forth64S/promram.4 \
 ../disk_nmt/ForthSrc/src_PRIMITIVES.4 \
 ../disk_nmt/ForthSrc/src_HPROC.4 \
 ../disk_nmt/ForthSrc/src_VARS.4 \
 ../disk_nmt/ForthSrc/macroopt.4 \
 ../disk_nmt/ForthSrc/NUMB_PARSE_IO.4 \
 ../disk_nmt/ForthSrc/LITERAL.4 \
 ../disk_nmt/ForthSrc/src_FIND_INTERP.4 \
 ../disk_nmt/ForthSrc/TERM/INCLUDE.4 \
 ../disk_nmt/ForthSrc/fstart.4 \
 Forth64S/Meta_x86_64/SRC/gasm64.4th \
 Forth64S/Meta_x86_64/SRC/disgasm.4 \
 Forth64S/Meta_x86_64/SRC/tc.f \
 Forth64S/src/global.4 \
 Forth64S/Meta_x86_64/SRC/mlist.f \
 Forth64S/Meta_x86_64/SRC/lex.4th \
 Forth64S/MLibs.4 \
 Forth64S/Meta_x86_64/mhead0.f \
 Forth64S/Meta_x86_64/SRC/macroopt.4 


all:	$(TARGETS)

forth.so : forth.o tmpamain.o

clean:
	rm -f $(TARGETS) *~ *.o *.so tmpamain.S
	rm -f $(SRCDIR)/Forth64S/Meta_x86_64/Mak64CppForth $(SRCDIR)/Forth64S/Meta_x86_64/MetaForth
	rm -f $(SRCDIR)/Forth64S/src/*.S

install:
	mkdir -p $(INSTALLROOT)$(APPSDIR)
	$(INSTALL) -m 644 $(TARGETS) $(INSTALLROOT)$(APPSDIR)

.PHONY: install

#include $(SRCDIR)/../Make.rules

%.efi: %.so
	$(OBJCOPY) -j .text -j .sdata -j .data -j .dynamic -j .rodata -j .rel \
		    -j .rela -j .rel.* -j .rela.* -j .rel* -j .rela* \
		    -j .areloc -j .reloc $(FORMAT) $*.so $@

%.so: %.o
	$(LD) $(LDFLAGS) $^ -o $@ $(LOADLIBES)

%.o: %.c
	$(CC) $(INCDIR) $(CFLAGS) -fno-inline-functions $(CPPFLAGS) -c $< -o $@

%.s: %.c
	$(CC) $(INCDIR) $(CFLAGS) $(CPPFLAGS) -S $< -o $@

%.i: %.c
	$(CC) $(INCDIR) $(CFLAGS) $(CPPFLAGS) -E $< -o $@

%.o: %.S
	$(CC) $(INCDIR) $(CFLAGS) $(CPPFLAGS) -c $< -o $@

%.s: %.S
	$(CC) $(INCDIR) $(CFLAGS) $(CPPFLAGS) -E $< -o $@


$(SRCDIR)/Forth64S/Meta_x86_64/MetaForth: $(SRCDIR)/Forth64S/Meta_x86_64/Mak64CppForth
	cp $< $@

$(SRCDIR)/Forth64S/Meta_x86_64/Mak64CppForth: Forth64S/Meta_x86_64/Mak64Forth.cpp 
	g++ -o $@ $< -Wreturn-type

tmpamain.S: $(FORTH_SRC) Forth64S/amain.S Forth64S/Meta_x86_64/MetaForth
	echo "qwerty"
	echo $(SRCDIR)/Forth64S/amain.S
	$(SRCDIR)/sgen.sh 
	cat $(SRCDIR)/Forth64S/amain.S > tmpamain.S  
