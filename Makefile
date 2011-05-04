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

# $Id: Makefile 1443 2010-03-20 17:58:38Z simonjwright $

# Top-level (GNU) Makefile for Booch Components. Used for distribution
# construction.

all:
	@echo "'make libs' to make the BC libraries for use with bc.gpr."
	@echo "'make dist' to make the distribution."

libs:
	gnatmake -p -P bc -Xstyle=release
	gnatmake -p -P bc -Xstyle=debug

SUBDIRS = src tests demos GNAT html

# Create the current date, in the form yyyymmdd. This certainly works
# in Ubuntu Linux 8.04 & Mac OS X.
DATE ?= $(shell date +%Y%m%d)$(SUBRELEASE)

DISTRIBUTION_FILES =	\
bc-$(DATE).tgz		\
bc-$(DATE).tar.bz2	\
bc-$(DATE).zip		\
bc-html-$(DATE).zip

ifneq ($(shell which 7za),)
  DISTRIBUTION_FILES += bc-$(DATE).7z
endif

dist: COPYING README Makefile bc.gpr $(DISTRIBUTION_FILES)
	-@rm -rf $@
	mkdir -p $@
	cp -p $^ $@/
	cp -pR contrib $@/

bc-$(DATE): force
	-rm -rf $@
	mkdir $@
	$(MAKE) DIST=$(PWD)/$@ -C src dist
	$(MAKE) DIST=$(PWD)/$@ -C tests dist
	$(MAKE) DIST=$(PWD)/$@ -C demos dist
	$(MAKE) DIST=$(PWD)/$@ -C GNAT dist

bc-$(DATE).tgz: bc-$(DATE)
	tar zcvf $@ $</

bc-$(DATE).tar.bz2: bc-$(DATE)
	tar --create --verbose --bzip2 --file=$@ $</

bc-$(DATE).zip: bc-$(DATE)
	zip -lr $@ $</*

bc-$(DATE).7z: bc-$(DATE)
	7za a -r $@ $</

bc-html-$(DATE).zip: force
	$(MAKE) DIST=$(PWD)/$@ -C html dist


.PHONY: force

# vim: textwidth=0 nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab
# vim: filetype=make encoding=utf-8 fileformat=unix
