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

MEMORY_CHECK = -largs ~/lib/ccmalloc.o -ldl

test:: $(TESTABLE)
	for t in $(TESTABLE); do \
	  gnatmake -g -gnata $$t $(MEMORY_CHECK); \
	done
	for t in $(TESTABLE); do \
	  echo running $$t; \
	  ./$$t 2>$$t.log; \
	done

TESTABLE += list_test
TESTABLE += queue_test
TESTABLE += smart_test
TESTABLE += stack_test
TESTABLE += tree_test

DISTRIBUTION_FILES = \
bc.tar.gz \
bc.zip \
bc.src \
ACL \
README

dist: $(DISTRIBUTION_FILES)
	-@rm -rf distribution
	mkdir -p distribution/download
	cp -p $(DISTRIBUTION_FILES) distribution/download/
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
bc-smart.ad[bs] \
bc-support-bounded.ad[bs] \
bc-support-dynamic.ad[bs] \
bc-support-nodes.ad[bs] \
bc-support-unbounded.ad[bs] \
bc-support.ad[bs] \
bc.ad[bs] \
character_references.ad[bs] \
list_test.ad[bs] \
queue_test.ad[bs] \
root_bin_trees.ad[bs] \
root_bounded.ad[bs] \
root_bounded_queues.ad[bs] \
root_bounded_stacks.ad[bs] \
root_container.ad[bs] \
root_dlist.ad[bs] \
root_dynamic.ad[bs] \
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
root_unbounded.ad[bs] \
root_unbounded_queues.ad[bs] \
root_unbounded_stacks.ad[bs] \
smart_test.ad[bs] \
stack_test.ad[bs] \
tree_test.ad[bs]

SOURCE = makefile ACL README $(ADA_SOURCE)

bc.tar.gz: $(SOURCE)
	tar zcvf bc.tar.gz $(SOURCE)

bc.zip: $(SOURCE)
	zip -l bc.zip $(SOURCE)

bc.src: $(ADA_SOURCE)
	cat $(ADA_SOURCE) > bc.src
