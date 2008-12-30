# Copyright (C) Simon Wright <simon@pushface.org>. All Rights Reserved.
#
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

# (GNU) Makefile for test of Booch Components distribution.

ifneq (, $(findstring CYGWIN, $(sys)))
  EXE = .exe
else
  EXE =
endif

GNATMAKE=gnatmake

#####################
# Default build rule
%$(EXE): %.adb
	$(GNATMAKE) -Ptests $<

########
# Tests

test: testable
	for t in $(TESTABLE); do \
	  echo running $$t; \
	  ./$$t 2>&1; \
	done

TESTABLE += avl_test$(EXE)
avl_test$(EXE): force

TESTABLE += bag_test$(EXE)
bag_test$(EXE): force

TESTABLE += collection_test$(EXE)
collection_test$(EXE): force

TESTABLE += graph_test$(EXE)
graph_test$(EXE): force

TESTABLE += list_test$(EXE)
list_test$(EXE): force

TESTABLE += map_test$(EXE)
map_test$(EXE): force

TESTABLE += ordered_collection_test$(EXE)
ordered_collection_test$(EXE): force

TESTABLE += ordered_queue_test$(EXE)
ordered_queue_test$(EXE): force

TESTABLE += queue_test$(EXE)
queue_test$(EXE): force

TESTABLE += ring_test$(EXE)
ring_test$(EXE): force

TESTABLE += set_test$(EXE)
set_test$(EXE): force

TESTABLE += smart_test$(EXE)
smart_test$(EXE): force

TESTABLE += sort_test$(EXE)
sort_test$(EXE): force

TESTABLE += stack_test$(EXE)
stack_test$(EXE): force

TESTABLE += stream_test$(EXE)
stream_test$(EXE): force

TESTABLE += tree_test$(EXE)
tree_test$(EXE): force

testable: .build $(TESTABLE)

.build:
	mkdir $@

.PHONY: force
