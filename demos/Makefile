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

# (GNU) Makefile for demos of Booch Components distribution.

########
# Demos

ifneq (, $(findstring CYGWIN, $(sys)))
  EXE = .exe
else
  EXE =
endif

GNATMAKE = gnatmake

#####################
# Default build rule
%$(EXE): %.adb
	$(GNATMAKE) -Pdemos $<

all:: demos

DEMOS += ada_units$(EXE)
ada_units$(EXE): force

DEMOS += configuration_demo$(EXE)
configuration_demo$(EXE): force

DEMOS += lists_traversal$(EXE)
lists_traversal$(EXE): force

DEMOS += ordering_test$(EXE)
ordering_test$(EXE): force

DEMOS += storage$(EXE)
storage$(EXE): force

DEMOS += storage_timing$(EXE)
storage_timing$(EXE): force

DEMOS += test_synchronization$(EXE)
test_synchronization$(EXE): force

DEMOS += time_change$(EXE)
time_change$(EXE): force

DEMOS += time_collections$(EXE)
time_collections$(EXE): force

DEMOS += time_lists$(EXE)
time_lists$(EXE): force

DEMOS += time_queues$(EXE)
time_queues$(EXE): force

DEMOS += user_map$(EXE)
user_map$(EXE): force

DEMOS += user_set$(EXE)
user_set$(EXE): force

DEMOS += word_count$(EXE)
word_count$(EXE): bcwords.ada force
	gnatchop -r -w bcwords.ada
	$(GNATMAKE) -Pdemos word_count

demos: .build $(DEMOS)

.build:
	mkdir .build

.PHONY: force
