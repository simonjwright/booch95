# Copyright 1998 Simon Wright. All Rights Reserved.
#      This program is free software; you can redistribute it
#      and/or modify it under the terms of the Ada Community
#      License which comes with this Library.
#
#      This program is distributed in the hope that it will be
#      useful, but WITHOUT ANY WARRANTY; without even the implied
#      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#      PURPOSE. See the Ada Community License for more details.
#      You should have received a copy of the Ada Community
#      License with this library, in the file named "Ada Community
#      License" or "ACL". If not, contact the author of this library 
#      for a copy.
#

# $Id$

# (GNU) Makefile for Booch Components.

all::

########
# Tests

# For checking memory leaks, I use ccmalloc from Armin Biere
# (armin@ira.uka.de + armin+@cs.cmu.edu, http://iseran.ira.uka.de/~armin)

MEMORY_CHECK = -largs ~/ccmalloc-0.2.3/src/libccmalloc.a -ldl

test:: $(TESTABLE)
	for t in $(TESTABLE); do \
	  gnatmake -g -gnata $$t $(MEMORY_CHECK); \
	done
	for t in $(TESTABLE); do \
	  echo running $$t; \
	  ./$$t 2>$$t.log; \
	done

TESTABLE += graph_test
TESTABLE += list_test
TESTABLE += queue_test
TESTABLE += smart_test
TESTABLE += stack_test
TESTABLE += tree_test

############################
# Distribution construction

# Create the current date, in the form yyyymmdd. This certainly works in Linux.
DATE = $(shell date +%Y%m%d)

DISTRIBUTION_FILES = \
bc-$(DATE).tgz \
bc-$(DATE).zip \
bc-$(DATE).src \
ACL

dist: $(DISTRIBUTION_FILES)
	-@rm -rf distribution
	mkdir distribution
	(cd distribution; \
	 ln -s ../../exported-booch-components download; \
	 ln -s ../../wavefront-booch-components wavefront)
	cp -p $(DISTRIBUTION_FILES) WAVEFRONT-README distribution/wavefront/
	cp -p ACL html/*.html html/*.gif distribution/

ADA_SOURCE = \
bc-containers-lists-double.ad[bs] \
bc-containers-lists-single.ad[bs] \
bc-containers-lists.ad[bs] \
bc-containers-queues-bounded.ad[bs] \
bc-containers-queues-dynamic.ad[bs] \
bc-containers-queues-unbounded.ad[bs] \
bc-containers-queues.ad[bs] \
bc-containers-stacks-bounded.ad[bs] \
bc-containers-stacks-dynamic.ad[bs] \
bc-containers-stacks-unbounded.ad[bs] \
bc-containers-stacks.ad[bs] \
bc-containers-trees-binary.ad[bs] \
bc-containers-trees-multiway.ad[bs] \
bc-containers-trees.ad[bs] \
bc-containers.ad[bs] \
bc-graphs.ad[bs] \
bc-graphs-directed.ad[bs] \
bc-graphs-undirected.ad[bs] \
bc-smart.ad[bs] \
bc-support-bounded.ad[bs] \
bc-support-dynamic.ad[bs] \
bc-support-exceptions.ad[bs] \
bc-support-managed_storage.ad[bs] \
bc-support-nodes.ad[bs] \
bc-support-unbounded.ad[bs] \
bc-support-unmanaged_storage.ad[bs] \
bc-support.ad[bs] \
bc.ad[bs] \
character_references.ad[bs] \
global_heap.ads \
graph_test.adb \
graph_test_support.ads \
lists_for_timing.ads \
list_test.ad[bs] \
queues_for_timing.ads \
queue_test.ad[bs] \
root_bin_trees.ad[bs] \
root_bounded_queues.ad[bs] \
root_bounded_stacks.ad[bs] \
root_container.ad[bs] \
root_dlist.ad[bs] \
root_dynamic_queues.ad[bs] \
root_dynamic_stacks.ad[bs] \
root_lists.ad[bs] \
root_mway_trees.ad[bs] \
root_queues.ad[bs] \
root_slist.ad[bs] \
root_smart.ad[bs] \
root_smart_pointer.ad[bs] \
root_stacks.ad[bs] \
root_trees.ad[bs] \
root_unbounded_queues.ad[bs] \
root_unbounded_stacks.ad[bs] \
smart_test.ad[bs] \
stack_test.ad[bs] \
storage.adb \
time_lists.adb \
time_queues.adb \
tree_test.ad[bs]

SOURCE = makefile ACL README $(ADA_SOURCE)

bc-$(DATE): force
	-rm -rf $@
	mkdir $@
	cp -p $(SOURCE) $@/

bc-$(DATE).tgz: bc-$(DATE)
	tar zcvf $@ $</

bc-$(DATE).zip: bc-$(DATE)
	zip -l $@ $</*

bc-$(DATE).src: $(ADA_SOURCE)
	cat $(ADA_SOURCE) > $@

.PHONY: force
