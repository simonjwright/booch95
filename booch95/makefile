# Copyright 1998-2003 Simon Wright <simon@pushface.org>

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

# $Id$

# (GNU) Makefile for Booch Components.

all::

#####################
# Default build rule
%: %.adb
	gnatmake $(GNATMAKE_FLAGS) $@

########
# Tests

#GCOV_FLAGS = -ftest-coverage -fprofile-arcs

GNATMAKE_FLAGS = -O2 -g -gnatqQafy -gnatwaL -j2 -m -k
GNATMAKE_FLAGS += $(GCOV_FLAGS)

ADA_SOURCE = \
bc.ad[bs] \
bc-containers.ad[bs] \
bc-copy.ad[bs] \
bc-filter.ad[bs]

ADA_SOURCE += \
bc-support-bounded.ad[bs] \
bc-support-bounded_hash_tables.ad[bs] \
bc-support-dynamic.ad[bs] \
bc-support-exceptions.ad[bs] \
bc-support-hash_tables.ad[bs] \
bc-support-managed_storage.ad[bs] \
bc-support-memory_streams.ad[bs] \
bc-support-standard_storage.ads \
bc-support-synchronization.ad[bs] \
bc-support-unbounded.ad[bs] \
bc-support-unmanaged.ad[bs] \
bc-support-unmanaged_storage.ad[bs] \
bc-support.ad[bs]

test: testable
	for t in $(TESTABLE); do \
	  echo running $$t; \
	  ./$$t 2>&1; \
	done
clean::
	rm -f $(TESTABLE)

TEST_SRC = assertions.ad[bs] global_heap.ad[bs]

TESTABLE += avl_test
TEST_SRC += avl_test.ad[bs] avl_test_support.ad[bs]
avl_test: force

ADA_SOURCE += \
bc-containers-bags-bounded.ad[bs] \
bc-containers-bags-dynamic.ad[bs] \
bc-containers-bags-unbounded.ad[bs] \
bc-containers-bags-unmanaged.ad[bs] \
bc-containers-bags.ad[bs]
TESTABLE += bag_test
TEST_SRC += bag_test.ad[bs] bag_test_support.ad[bs]
bag_test: force

ADA_SOURCE += \
bc-containers-collections-bounded.ad[bs] \
bc-containers-collections-dynamic.ad[bs] \
bc-containers-collections-unbounded.ad[bs] \
bc-containers-collections-unmanaged.ad[bs] \
bc-containers-collections.ad[bs]
TESTABLE += collection_test
TEST_SRC += collection_test.ad[bs] collection_test_support.ad[bs]
collection_test: force

ADA_SOURCE += \
bc-containers-deques-bounded.ad[bs] \
bc-containers-deques-dynamic.ad[bs] \
bc-containers-deques-unbounded.ad[bs] \
bc-containers-deques.ad[bs]
TESTABLE += deque_test
TEST_SRC += deque_test.ad[bs] deque_test_support.ad[bs]
deque_test: force

ADA_SOURCE += \
bc-graphs.ad[bs] \
bc-graphs-directed.ad[bs] \
bc-graphs-undirected.ad[bs]
TESTABLE += graph_test
TEST_SRC += graph_test.ad[bs] graph_test_support.ad[bs]
graph_test: force

ADA_SOURCE += \
bc-containers-lists-double.ad[bs] \
bc-containers-lists-single.ad[bs] \
bc-containers-lists.ad[bs]
TESTABLE += list_test
TEST_SRC += list_test.ad[bs] list_test_support.ad[bs]
list_test: force

ADA_SOURCE += \
bc-containers-maps-bounded.ad[bs] \
bc-containers-maps-dynamic.ad[bs] \
bc-containers-maps-unbounded.ad[bs] \
bc-containers-maps-unmanaged.ad[bs] \
bc-containers-maps.ad[bs]
TESTABLE += map_test
TEST_SRC += map_test.ad[bs] map_test_support.ad[bs] chunks.ad[bs]
map_test: force

ADA_SOURCE += \
bc-containers-collections-ordered-bounded.ad[bs] \
bc-containers-collections-ordered-dynamic.ad[bs] \
bc-containers-collections-ordered-unbounded.ad[bs] \
bc-containers-collections-ordered-unmanaged.ad[bs] \
bc-containers-collections-ordered.ad[bs]
TESTABLE += ordered_collection_test
TEST_SRC += ordered_collection_test.ad[bs] ordered_collection_test_support.ad[bs]
ordered_collection_test: force

ADA_SOURCE += \
bc-containers-queues-ordered.ad[bs] \
bc-containers-queues-ordered-bounded.ad[bs] \
bc-containers-queues-ordered-dynamic.ad[bs] \
bc-containers-queues-ordered-unbounded.ad[bs] \
bc-containers-queues-ordered-unmanaged.ad[bs]
TESTABLE += ordered_queue_test
TEST_SRC += ordered_queue_test.ad[bs] ordered_queue_test_support.ad[bs]
ordered_queue_test: force

ADA_SOURCE += \
bc-containers-queues-bounded.ad[bs] \
bc-containers-queues-dynamic.ad[bs] \
bc-containers-queues-unbounded.ad[bs] \
bc-containers-queues-unmanaged.ad[bs] \
bc-containers-queues.ad[bs]
TESTABLE += queue_test
TEST_SRC += queue_test.ad[bs] queue_test_support.ad[bs]
queue_test: force

ADA_SOURCE += \
bc-containers-rings.ad[bs] \
bc-containers-rings-bounded.ad[bs] \
bc-containers-rings-dynamic.ad[bs] \
bc-containers-rings-unbounded.ad[bs]
TESTABLE += ring_test
TEST_SRC += ring_test.ad[bs] ring_test_support.ad[bs]
ring_test: force

ADA_SOURCE += \
bc-containers-sets-bounded.ad[bs] \
bc-containers-sets-dynamic.ad[bs] \
bc-containers-sets-unbounded.ad[bs] \
bc-containers-sets.ad[bs]
TESTABLE += set_test
TEST_SRC += set_test.ad[bs] set_test_support.ad[bs]
set_test: force

ADA_SOURCE += \
bc-smart.ad[bs] bc-support-smart_pointers.ad[bs]
TESTABLE += smart_test
TEST_SRC += smart_test.ad[bs] smart_test_support.ad[bs]
smart_test: force

ADA_SOURCE += \
bc-containers-quicksort.ad[bs] \
bc-containers-shellsort.ad[bs]
TESTABLE += sort_test
TEST_SRC += sort_test.adb
sort_test: force

ADA_SOURCE += \
bc-containers-stacks-bounded.ad[bs] \
bc-containers-stacks-dynamic.ad[bs] \
bc-containers-stacks-unbounded.ad[bs] \
bc-containers-stacks-unmanaged.ad[bs] \
bc-containers-stacks.ad[bs]
TESTABLE += stack_test
TEST_SRC += stack_test.ad[bs] stack_test_support.ad[bs]
stack_test: force

TESTABLE += stream_test
TEST_SRC += stream_test.ad[bs] stream_test_support.ad[bs] \
stream_test_support-tc[bdu].ad[bs]
stream_test: force

ADA_SOURCE += \
bc-containers-trees-avl.ad[bs] \
bc-containers-trees-avl-print.ad[bs] \
bc-containers-trees-avl-validate.ad[bs] \
bc-containers-trees-binary.ad[bs] \
bc-containers-trees-binary-in_order.ad[bs] \
bc-containers-trees-binary-post_order.ad[bs] \
bc-containers-trees-binary-pre_order.ad[bs] \
bc-containers-trees-multiway.ad[bs] \
bc-containers-trees-multiway-post_order.ad[bs] \
bc-containers-trees-multiway-pre_order.ad[bs] \
bc-containers-trees.ad[bs]
TESTABLE += tree_test
TEST_SRC += tree_test.ad[bs] tree_test_support.ad[bs]
tree_test: force

testable: $(TESTABLE)

########
# Demos

DEMO_SRC = global_heap.ad[bs]

DEMOS += ada_units
DEMO_SRC += ada_units.ad[bs] ada_unit_support.ad[bs]
ada_units: force

DEMOS += configuration_demo
DEMO_SRC += configuration_demo.ad[bs] configuration_demo_support.ad[bs]
configuration_demo: force

DEMOS += lists_traversal
DEMO_SRC += lists_for_traversal.ad[bs] lists_traversal.ad[bs]
lists_traversal: force

DEMOS += ordering_test
DEMO_SRC += ordering_support.ad[bs] ordering_test.ad[bs]
ordering_test: force

DEMOS += storage
DEMO_SRC += storage.ad[bs]
storage: force

DEMOS += test_synchronization
DEMO_SRC += test_synchronization.ad[bs]
test_synchronization: force

DEMOS += time_lists
DEMO_SRC += time_lists.ad[bs] lists_for_timing.ad[bs]
time_lists: force

DEMOS += time_queues
DEMO_SRC += time_queues.ad[bs] queues_for_timing.ad[bs]
time_queues: force

DEMOS += unmanaged_storage
DEMO_SRC += unmanaged_storage.ad[bs]
unmanaged_storage: force

DEMOS += user_map
DEMO_SRC += user_map.ad[bs] user_map_support.ad[bs]
user_map: force

DEMOS += user_set
DEMO_SRC += user_set.ad[bs] user_set_support.ad[bs]
user_set: force

DEMOS += word_count
DEMO_SRC += bcwords.ada
word_count: bcwords.ada
	gnatchop -r -w bcwords.ada
	gnatmake $(GNATMAKE_FLAGS) $@

demos: $(DEMOS)

clean::
	rm -f $(DEMOS)

############################
# Distribution construction

# Create the current date, in the form yyyymmdd. This certainly works in Linux.
DATE = $(shell date +%Y%m%d)$(SUBRELEASE)

DISTRIBUTION_FILES = \
bc-$(DATE).tgz \
bc-$(DATE).zip \
bc-$(DATE).src \
bc-html-$(DATE).zip \
COPYING

dist: $(DISTRIBUTION_FILES)
	-@rm -rf distribution
	mkdir distribution
	(cd distribution; \
	 ln -s $(HOME)/exported-booch-components download; \
	 ln -s $(HOME)/wavefront-booch-components wavefront; \
	 ln -s ../contrib)
	cp -p $(DISTRIBUTION_FILES) distribution/wavefront/
	cp -p README distribution/download/
	cp -p WAVEFRONT-README distribution/wavefront/README
	cp -p COPYING html/*.{html,gif,jpg,png} html/x.ada \
	  html/coldframe-hash.{tgz,zip} distribution/

SOURCE = makefile COPYING README $(ADA_SOURCE)

bc-$(DATE): force
	-rm -rf $@
	mkdir $@
	tar cf - $(SOURCE) | (cd $@; tar xvf -)
	mkdir $@/test
	cp makefile-test $@/test/makefile
	tar cf - $(TEST_SRC) | (cd $@/test; tar xvf -)
	mkdir $@/demo
	cp makefile-demo $@/demo/makefile
	tar cf - $(DEMO_SRC) | (cd $@/demo; tar xvf -)

bc-$(DATE).tgz: bc-$(DATE)
	tar zcvf $@ $</

bc-$(DATE).zip: bc-$(DATE)
	zip -lr $@ $</*

bc-$(DATE).src: $(ADA_SOURCE) $(TEST_SRC) $(DEMO_SRC)
	cat $(ADA_SOURCE) $(TEST_SRC) $(DEMO_SRC) > $@

bc-html-$(DATE).zip: force
	(cd html; zip ../$@ x.ada *.gif *.jpg *.html coldframe-hash.{tgz,zip})

.PHONY: force
