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

test:: $(TESTABLE)
	for t in $(TESTABLE); do \
	  gnatmake -g -gnata $$t; \
	done
	for t in $(TESTABLE); do \
	  echo running $$t; \
	  $$t; \
	done

TESTABLE += list_test
TESTABLE += queue_test
TESTABLE += stack_test
TESTABLE += tree_test

