# Copyright (C) 1998-1999 Simon Wright. All Rights Reserved.
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
# The -Wl,-u,malloc forces the malloc symbol to be required so as to ensure
# that it's satisfied from libccmalloc.a.

MEMORY_CHECK = \
  -largs -Wl,-u,malloc $(HOME)/ccmalloc-0.2.3/src/libccmalloc.a -ldl
GNATMAKE_FLAGS = -O3 -g -gnata -gnatwu

test: testable
	for t in $(TESTABLE); do \
	  echo running $$t; \
	  ./$$t 2>$$t.log; \
	done

TESTABLE += bag_test
bag_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += collection_test
collection_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += graph_test
graph_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += list_test
list_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += map_test
map_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += ordered_collection_test
ordered_collection_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += ordered_queue_test
ordered_queue_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += queue_test
queue_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += ring_test
ring_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += smart_test
smart_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += set_test
set_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += stack_test
stack_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

TESTABLE += tree_test
tree_test: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

testable: $(TESTABLE)

# Aonix special tests (included here so we check they will at least compile
# under Linux)

aonix_test: aonix_testable
	for t in $(AONIX_TESTABLE); do \
	  echo running $$t; \
	  ./$$t 2>$$t.log; \
	done

AONIX_TESTABLE += bag_test_aonix
bag_test_aonix: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

AONIX_TESTABLE += collection_test_aonix
collection_test_aonix: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

AONIX_TESTABLE += map_test_aonix
map_test_aonix: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

#AONIX_TESTABLE += ordered_collection_test_aonix
#ordered_collection_test_aonix: force
#	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

#AONIX_TESTABLE += ordered_queue_test_aonix
#ordered_queue_test_aonix: force
#	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

AONIX_TESTABLE += queue_test_aonix
queue_test_aonix: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

AONIX_TESTABLE += set_test_aonix
set_test_aonix: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

AONIX_TESTABLE += stack_test_aonix
stack_test_aonix: force
	gnatmake $(GNATMAKE_FLAGS) $@ $(MEMORY_CHECK)

aonix_testable: $(AONIX_TESTABLE)

########
# Demos

DEMOS += ada_units
DEMOS += lists_traversal
DEMOS += ordering_test
DEMOS += test_synchronization
DEMOS += time_lists
DEMOS += time_queues
DEMOS += unmanaged_storage
DEMOS += user_set
DEMOS += word_count

ada_units: force
	gnatmake $(GNATMAKE_FLAGS) $@

lists_traversal: force
	gnatmake $(GNATMAKE_FLAGS) $@

ordering_test: force
	gnatmake $(GNATMAKE_FLAGS) $@

test_synchronization: force
	gnatmake $(GNATMAKE_FLAGS) $@

time_lists: force
	gnatmake $(GNATMAKE_FLAGS) $@

time_queues: force
	gnatmake $(GNATMAKE_FLAGS) $@

unmanaged_storage: force
	gnatmake $(GNATMAKE_FLAGS) $@

user_set: force
	gnatmake $(GNATMAKE_FLAGS) $@

word_count: bcwords.ada
	gnatchop -w bcwords.ada
	gnatmake $(GNATMAKE_FLAGS) $@

demos: $(DEMOS)

############################
# Distribution construction

# Create the current date, in the form yyyymmdd. This certainly works in Linux.
DATE = $(shell date +%Y%m%d)

DISTRIBUTION_FILES = \
bc-$(DATE).tgz \
bc-$(DATE).zip \
bc-$(DATE).src \
bc-html-$(DATE).zip \
ACL

dist: $(DISTRIBUTION_FILES)
	-@rm -rf distribution
	mkdir distribution
	(cd distribution; \
	 ln -s ../../exported-booch-components download; \
	 ln -s ../../wavefront-booch-components wavefront; \
	 ln -s ../contrib)
	cp -p $(DISTRIBUTION_FILES) distribution/wavefront/
	cp -p README distribution/download/
	cp -p WAVEFRONT-README distribution/wavefront/README
	cp -p ACL html/*.html html/*.gif distribution/

ADA_SOURCE = \
ada_units.ad[bs] \
ada_unit_support.ad[bs] \
bag_test.ad[bs] \
bag_test_aonix.ad[bs] \
bag_test_support.ad[bs] \
bc-containers-bags-bounded.ad[bs] \
bc-containers-bags-dynamic.ad[bs] \
bc-containers-bags-unbounded.ad[bs] \
bc-containers-bags.ad[bs] \
bc-containers-collections-ordered-unbounded.ad[bs] \
bc-containers-collections-ordered.ad[bs] \
bc-containers-collections-unbounded.ad[bs] \
bc-containers-collections.ad[bs] \
bc-containers-lists-double.ad[bs] \
bc-containers-lists-single.ad[bs] \
bc-containers-lists.ad[bs] \
bc-containers-maps-bounded.ad[bs] \
bc-containers-maps-dynamic.ad[bs] \
bc-containers-maps-unbounded.ad[bs] \
bc-containers-maps.ad[bs] \
bc-containers-queues-bounded.ad[bs] \
bc-containers-queues-dynamic.ad[bs] \
bc-containers-queues-ordered.ad[bs] \
bc-containers-queues-ordered-unbounded.ad[bs] \
bc-containers-queues-unbounded.ad[bs] \
bc-containers-queues.ad[bs] \
bc-containers-rings.ad[bs] \
bc-containers-rings-unbounded.ad[bs] \
bc-containers-rings-unbounded-guarded.ad[bs] \
bc-containers-rings-unbounded-synchronized.ad[bs] \
bc-containers-sets-bounded.ad[bs] \
bc-containers-sets-dynamic.ad[bs] \
bc-containers-sets-unbounded.ad[bs] \
bc-containers-sets.ad[bs] \
bc-containers-stacks-bounded.ad[bs] \
bc-containers-stacks-dynamic.ad[bs] \
bc-containers-stacks-unbounded.ad[bs] \
bc-containers-stacks.ad[bs] \
bc-containers-trees-avl.ad[bs] \
bc-containers-trees-avl-print.ad[bs] \
bc-containers-trees-binary.ad[bs] \
bc-containers-trees-binary-in_order.ad[bs] \
bc-containers-trees-binary-post_order.ad[bs] \
bc-containers-trees-binary-pre_order.ad[bs] \
bc-containers-trees-multiway.ad[bs] \
bc-containers-trees-multiway-post_order.ad[bs] \
bc-containers-trees-multiway-pre_order.ad[bs] \
bc-containers-trees.ad[bs] \
bc-containers.ad[bs] \
bc-graphs.ad[bs] \
bc-graphs-directed.ad[bs] \
bc-graphs-undirected.ad[bs] \
bc-smart.ad[bs] \
bc-support-bounded.ad[bs] \
bc-support-dynamic.ad[bs] \
bc-support-exceptions.ad[bs] \
bc-support-hash_tables.ad[bs] \
bc-support-managed_storage.ad[bs] \
bc-support-nodes.ad[bs] \
bc-support-synchronization.ad[bs] \
bc-support-unbounded.ad[bs] \
bc-support-unmanaged_storage.ad[bs] \
bc-support.ad[bs] \
bc.ad[bs] \
bcwords.ada \
chunks.ad[bs] \
collection_test.ad[bs] \
collection_test_aonix.ad[bs] \
collection_test_support.ad[bs] \
global_heap.ad[bs] \
graph_test.ad[bs] \
graph_test_support.ad[bs] \
lists_for_timing.ad[bs] \
lists_for_traversal.ad[bs] \
lists_traversal.ad[bs] \
list_test.ad[bs] \
list_test_support.ad[bs] \
map_test.ad[bs] \
map_test_aonix.ad[bs] \
map_test_support.ad[bs] \
ordered_collection_test.ad[bs] \
ordered_collection_test_support.ad[bs] \
ordering_support.ad[bs] \
ordering_test.ad[bs] \
queues_for_timing.ad[bs] \
queue_test.ad[bs] \
queue_test_aonix.ad[bs] \
queue_test_support.ad[bs] \
ring_test.ad[bs] \
ring_test_support.ad[bs] \
smart_test.ad[bs] \
smart_test_support.ad[bs] \
set_test.ad[bs] \
set_test_aonix.ad[bs] \
set_test_support.ad[bs] \
stack_test.ad[bs] \
stack_test_aonix.ad[bs] \
stack_test_support.ad[bs] \
storage.ad[bs] \
test_synchronization.ad[bs] \
time_lists.ad[bs] \
time_queues.ad[bs] \
tree_test.ad[bs] \
tree_test_support.ad[bs] \
unmanaged_storage.ad[bs] \
user_set.ad[bs] \
user_set_support.ad[bs]

SOURCE = makefile ACL README $(ADA_SOURCE)

bc-$(DATE): force
	-rm -rf $@
	mkdir $@
	tar cf - $(SOURCE) | (cd $@; tar xvf -)

bc-$(DATE).tgz: bc-$(DATE)
	tar zcvf $@ $</

bc-$(DATE).zip: bc-$(DATE)
	zip -lr $@ $</*

bc-$(DATE).src: $(ADA_SOURCE)
	cat $(ADA_SOURCE) > $@

bc-html-$(DATE).zip: force
	(cd html; zip ../$@ *.gif *.html)

.PHONY: force
