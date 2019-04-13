# Copyright (C) Simon Wright <simon@pushface.org>.

# This package is free software; you can redistribute it and/or modify
# it under terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version. This package is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
# the GNU General Public License for more details. You should have
# received a copy of the GNU General Public License distributed with
# this package; see file COPYING.  If not, write to the Free Software
# Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

# (GNU) Makefile for test of Booch Components distribution.

ifneq (, $(findstring CYGWIN, $(sys)))
  EXE = .exe
else
  EXE =
endif

test: build-stamp
	for t in $(TESTABLE); do \
	  echo running $$t; \
	  ./$$t 2>&1 | tee $$t.log; \
	done

build-stamp: force
	gprbuild -p -P tests
	touch build-stamp

clean::
	rm -rf build-stamp

TESTABLE += avl_test$(EXE)
TEST_SRC += avl_test.ad[bs] avl_test_support.ad[bs]

TESTABLE += bag_test$(EXE)
TEST_SRC += bag_test.ad[bs] bag_test_support.ad[bs]

TESTABLE += collection_test$(EXE)
TEST_SRC += collection_test.ad[bs] collection_test_support.ad[bs]

TESTABLE += deque_test$(EXE)
TEST_SRC += deque_test.ad[bs] deque_test_support.ad[bs]

TESTABLE += graph_test$(EXE)
TEST_SRC += graph_test.ad[bs] graph_test_support.ad[bs]

TESTABLE += list_test$(EXE)
TEST_SRC += list_test.ad[bs] list_test_support.ad[bs]

TESTABLE += map_test$(EXE)
TEST_SRC += map_test.ad[bs] map_test_support.ad[bs] chunks.ad[bs]

TESTABLE += ordered_collection_test$(EXE)
TEST_SRC += ordered_collection_test.ad[bs] \
 ordered_collection_test_support.ad[bs]

TESTABLE += ordered_queue_test$(EXE)
TEST_SRC += ordered_queue_test.ad[bs] ordered_queue_test_support.ad[bs]

TESTABLE += queue_test$(EXE)
TEST_SRC += queue_test.ad[bs] queue_test_support.ad[bs]

TESTABLE += ring_test$(EXE)
TEST_SRC += ring_test.ad[bs] ring_test_support.ad[bs] \
tests-rings.ad[bs]

TESTABLE += set_test$(EXE)
TEST_SRC += set_test.ad[bs] set_test_support.ad[bs]

TESTABLE += smart_test$(EXE)
TEST_SRC += bc-support-smart_pointers-test_finalize.ad[bs]	\
smart_test.ad[bs]						\
smart_test_support.ad[bs]					\
tests-auto_pointers.ad[bs]

TESTABLE += sort_test$(EXE)
TEST_SRC += sort_test.adb

TESTABLE += stack_test$(EXE)
TEST_SRC += stack_test.ad[bs] stack_test_support.ad[bs]

TESTABLE += stream_test$(EXE)
TEST_SRC += stream_test.ad[bs] stream_test_support.ad[bs] \
stream_test_support-tc[bdu].ad[bs]

TESTABLE += tree_test$(EXE)
TEST_SRC += tree_test.ad[bs] tree_test_support.ad[bs] \
tests-avl_trees.ad[bs] tests-multiway_trees.ad[bs]

TEST_SRC += assertions.ad[bs]			\
global_heap.ad[bs]				\
tests-support.ad[bs] tests-items.ad[bs]		\
tests-main.adb tests.ads

dist:
	mkdir $(DIST)/tests
	cp $(TEST_SRC) Makefile tests.gpr AUnit_Tests.gpr $(DIST)/tests/

clean::
	gprclean -P tests.gpr

.PHONY: clean dist force
