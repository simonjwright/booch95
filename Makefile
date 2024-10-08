# Copyright (C) Simon Wright <simon@pushface.org>.

# This package is free software; you can redistribute it and/or
# modify it under terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2, or
# (at your option) any later version. This package is distributed in
# the hope that it will be useful, but WITHOUT ANY WARRANTY; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU General Public License for more
# details. You should have received a copy of the GNU General Public
# License distributed with this package; see file COPYING.  If not,
# write to the Free Software Foundation, 59 Temple Place - Suite
# 330, Boston, MA 02111-1307, USA.

# The top-level (GNU) Makefile for Booch Components. Used for
# installation and distribution construction.

# Compute the prefix of the current GNAT installation
prefix ?= $(realpath $(dir $(shell which gnatls))..)

# Work out where to install the GPR
debian = $(and $(wildcard /etc/debian_version),$(filter $(prefix),/usr))
GPR_INSTALL_SUBDIR = $(if $(debian),share/ada/adainclude,lib/gnat)

GPRBUILD ?= gprbuild
GPRCLEAN ?= gprclean

# 'make' to make the BC libraries for use with bc.gpr.
# 'make install' to install the BC libraries with your GNAT.
# 'make dist' to make the distribution.

libs: lib-static-stamp lib-relocatable-stamp

lib-static-stamp: force
	$(GPRBUILD) -p -Pbc -XLIBRARY_TYPE=static
	touch $@

lib-relocatable-stamp: force
	$(GPRBUILD) -p -Pbc -XLIBRARY_TYPE=relocatable
	touch $@

install: install-static install-relocatable

install-static: lib-static-stamp
	gprinstall					\
	  -f						\
	  --prefix=$(prefix)				\
	  -P bc.gpr					\
	  --install-name=bc				\
	  --project-subdir=$(GPR_INSTALL_SUBDIR)	\
	  -XLIBRARY_TYPE=static				\
	  --mode=dev					\
	  --create-missing-dirs				\
	  --build-var=LIBRARY_TYPE			\
	  --build-name=static

install-relocatable: lib-relocatable-stamp
	gprinstall					\
	  -f						\
	  --prefix=$(prefix)				\
	  -P bc.gpr					\
	  --install-name=bc				\
	  --project-subdir=$(GPR_INSTALL_SUBDIR)	\
	  -XLIBRARY_TYPE=relocatable			\
	  --mode=dev					\
	  --create-missing-dirs				\
	  --build-var=LIBRARY_TYPE			\
	  --build-name=relocatable

# Distribution construction

SUBDIRS = src tests demos GNAT html contrib

# Create the current date, in the form yyyymmdd. This certainly works
# in Ubuntu Linux 8.04 & Mac OS X.
DATE ?= $(shell date +%Y%m%d)$(SUBRELEASE)

TOP_LEVEL_FILES =				\
COPYING						\
INSTALL						\
Makefile					\
bc.gpr						\
debian-6.diff

DISTRIBUTION_FILES =	\
bc-$(DATE).tgz		\
bc-$(DATE).tar.bz2	\
bc-$(DATE).zip

ifneq ($(shell which 7za),)
  DISTRIBUTION_FILES += bc-$(DATE).7z
endif

dist: $(DISTRIBUTION_FILES)
	-@rm -rf $@
	mkdir -p $@
	cp -p $^ $@/

bc-$(DATE): force
	-rm -rf $@
	mkdir $@
	cp $(TOP_LEVEL_FILES) $@
	for s in $(SUBDIRS); do \
	  $(MAKE) DIST=$(PWD)/$@ -C $$s dist; \
	done

bc-$(DATE).tgz: bc-$(DATE)
	tar zcvf $@ $</

bc-$(DATE).tar.bz2: bc-$(DATE)
	tar --create --verbose --bzip2 --file=$@ $</

bc-$(DATE).zip: bc-$(DATE)
	zip -lr $@ $</*

bc-$(DATE).7z: bc-$(DATE)
	7za a -r $@ $</


.PHONY: dist force install			\
  lib-static-stamp lib-relocatable-stamp	\
  install-static install-relocatable

# vim: textwidth=0 nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab
# vim: filetype=make encoding=utf-8 fileformat=unix
