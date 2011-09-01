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

# $Id$

# (GNU) Makefile for Booch Components.

all::


#####################
# Default build rule
%: %.adb
	gnatmake $(GNATMAKE_FLAGS) $@

#GCOV_FLAGS = -O0 -ftest-coverage -fprofile-arcs

GNATMAKE_FLAGS = -O2 -g -gnatqQafoy -gnatwaL -j2 -m -k
GNATMAKE_FLAGS += $(GCOV_FLAGS)

ADA_SOURCE =					\
bc.ad[bs]					\
bc-containers.ad[bs]				\
bc-copy.ad[bs]					\
bc-filter.ad[bs]

ADA_SOURCE +=						\
bc-support-avl_trees.ad[bs]				\
bc-support-bounded.ad[bs]				\
bc-support-bounded_hash_tables.ad[bs]			\
bc-support-dynamic.ad[bs]				\
bc-support-exceptions.ad[bs]				\
bc-support-hash_tables.ad[bs]				\
bc-support-high_resolution_time-clock.adb		\
bc-support-high_resolution_time-clock.adb-pentium	\
bc-support-high_resolution_time-clock.adb-ppc32		\
bc-support-high_resolution_time.ad[bs]			\
bc-support-indefinite_bounded.ad[bs]			\
bc-support-indefinite_bounded_hash_tables.ad[bs]	\
bc-support-indefinite_hash_tables.ad[bs]		\
bc-support-indefinite_reference.ad[bs]			\
bc-support-indefinite_unbounded.ad[bs]			\
bc-support-indefinite_unmanaged.ad[bs]			\
bc-support-managed_storage.ad[bs]			\
bc-support-memory_streams.ad[bs]			\
bc-support-standard_storage.ads				\
bc-support-statistics.ad[bs]				\
bc-support-synchronization-debug.ad[bs]			\
bc-support-synchronization.ad[bs]			\
bc-support-unbounded.ad[bs]				\
bc-support-unmanaged.ad[bs]				\
bc-support-unmanaged_storage.ad[bs]			\
bc-support.ad[bs]

ADA_SOURCE +=					\
bc-containers-bags-bounded.ad[bs]		\
bc-containers-bags-dynamic.ad[bs]		\
bc-containers-bags-unbounded.ad[bs]		\
bc-containers-bags-unmanaged.ad[bs]		\
bc-containers-bags.ad[bs]

ADA_SOURCE +=					\
bc-containers-collections-bounded.ad[bs]	\
bc-containers-collections-dynamic.ad[bs]	\
bc-containers-collections-unbounded.ad[bs]	\
bc-containers-collections-unmanaged.ad[bs]	\
bc-containers-collections.ad[bs]

ADA_SOURCE +=					\
bc-containers-deques-bounded.ad[bs]		\
bc-containers-deques-dynamic.ad[bs]		\
bc-containers-deques-unbounded.ad[bs]		\
bc-containers-deques-unmanaged.ad[bs]		\
bc-containers-deques.ad[bs]

ADA_SOURCE +=					\
bc-graphs.ad[bs]				\
bc-graphs-directed.ad[bs]			\
bc-graphs-undirected.ad[bs]

ADA_SOURCE +=					\
bc-lists-double.ad[bs]				\
bc-lists-single.ad[bs]				\
bc-lists.ad[bs]					\
bc-containers-lists-double.ad[bs]		\
bc-containers-lists-single.ad[bs]		\
bc-containers-lists.ad[bs]

ADA_SOURCE +=					\
bc-containers-maps-bounded.ad[bs]		\
bc-containers-maps-dynamic.ad[bs]		\
bc-containers-maps-unbounded.ad[bs]		\
bc-containers-maps-unmanaged.ad[bs]		\
bc-containers-maps.ad[bs]

ADA_SOURCE +=						\
bc-containers-collections-ordered-bounded.ad[bs]	\
bc-containers-collections-ordered-dynamic.ad[bs]	\
bc-containers-collections-ordered-unbounded.ad[bs]	\
bc-containers-collections-ordered-unmanaged.ad[bs]	\
bc-containers-collections-ordered.ad[bs]

ADA_SOURCE +=					\
bc-containers-queues-ordered.ad[bs]		\
bc-containers-queues-ordered-bounded.ad[bs]	\
bc-containers-queues-ordered-dynamic.ad[bs]	\
bc-containers-queues-ordered-unbounded.ad[bs]	\
bc-containers-queues-ordered-unmanaged.ad[bs]

ADA_SOURCE +=					\
bc-containers-queues-bounded.ad[bs]		\
bc-containers-queues-dynamic.ad[bs]		\
bc-containers-queues-unbounded.ad[bs]		\
bc-containers-queues-unmanaged.ad[bs]		\
bc-containers-queues.ad[bs]

ADA_SOURCE +=					\
bc-containers-rings.ad[bs]			\
bc-containers-rings-bounded.ad[bs]		\
bc-containers-rings-dynamic.ad[bs]		\
bc-containers-rings-unbounded.ad[bs]		\
bc-containers-rings-unmanaged.ad[bs]

ADA_SOURCE +=					\
bc-containers-sets-bounded.ad[bs]		\
bc-containers-sets-dynamic.ad[bs]		\
bc-containers-sets-unbounded.ad[bs]		\
bc-containers-sets-unmanaged.ad[bs]		\
bc-containers-sets.ad[bs]

ADA_SOURCE +=					\
bc-containers-stacks-bounded.ad[bs]		\
bc-containers-stacks-dynamic.ad[bs]		\
bc-containers-stacks-unbounded.ad[bs]		\
bc-containers-stacks-unmanaged.ad[bs]		\
bc-containers-stacks.ad[bs]

ADA_SOURCE +=					\
bc-containers-trees-avl.ad[bs]			\
bc-containers-trees-avl-print.ad[bs]		\
bc-containers-trees-avl-validate.ad[bs]		\
bc-containers-trees-binary.ad[bs]		\
bc-containers-trees-binary-in_order.ad[bs]	\
bc-containers-trees-binary-post_order.ad[bs]	\
bc-containers-trees-binary-pre_order.ad[bs]	\
bc-containers-trees-multiway.ad[bs]		\
bc-containers-trees-multiway-post_order.ad[bs]	\
bc-containers-trees-multiway-pre_order.ad[bs]	\
bc-containers-trees.ad[bs]

ADA_SOURCE +=					\
bc-containers-quicksort.ad[bs]			\
bc-containers-shellsort.ad[bs]

ADA_SOURCE +=					\
bc-trees-avl_trees.ad[bs]			\
bc-trees-avl_trees-print.ad[bs]			\
bc-trees-avl_trees-validate.ad[bs]		\
bc-trees-binary_trees.ad[bs]			\
bc-trees-binary_trees-in_order.ad[bs]		\
bc-trees-binary_trees-post_order.ad[bs]		\
bc-trees-binary_trees-pre_order.ad[bs]		\
bc-trees-multiway_trees.ad[bs]			\
bc-trees-multiway_trees-post_order.ad[bs]	\
bc-trees-multiway_trees-pre_order.ad[bs]	\
bc-trees.ad[bs]

ADA_SOURCE +=					\
bc-support-auto_pointers.ad[bs]			\
bc-support-smart_pointers.ad[bs]

ADA_SOURCE +=					\
bc-simple_collections.ad[bs]			\
bc-trees-avl_trees-iterators.ad[bs]

ADA_SOURCE +=					\
bc-indefinite_containers.ad[bs]

ADA_SOURCE +=						\
bc-indefinite_containers-collections-bounded.ad[bs]	\
bc-indefinite_containers-collections-dynamic.ad[bs]	\
bc-indefinite_containers-collections-unbounded.ad[bs]	\
bc-indefinite_containers-collections-unmanaged.ad[bs]	\
bc-indefinite_containers-collections.ad[bs]

ADA_SOURCE +=							\
bc-indefinite_containers-collections-ordered-bounded.ad[bs]	\
bc-indefinite_containers-collections-ordered-dynamic.ad[bs]	\
bc-indefinite_containers-collections-ordered-unbounded.ad[bs]	\
bc-indefinite_containers-collections-ordered-unmanaged.ad[bs]	\
bc-indefinite_containers-collections-ordered.ad[bs]

ADA_SOURCE +=					\
bc-indefinite_containers-maps-bounded.ad[bs]	\
bc-indefinite_containers-maps-unbounded.ad[bs]	\
bc-indefinite_containers-maps.ad[bs]

ADA_SOURCE +=						\
bc-indefinite_containers-queues-bounded.ad[bs]		\
bc-indefinite_containers-queues-dynamic.ad[bs]		\
bc-indefinite_containers-queues-unbounded.ad[bs]	\
bc-indefinite_containers-queues-unmanaged.ad[bs]	\
bc-indefinite_containers-queues.ad[bs]

ADA_SOURCE +=							\
bc-indefinite_containers-queues-ordered-bounded.ad[bs]		\
bc-indefinite_containers-queues-ordered-dynamic.ad[bs]		\
bc-indefinite_containers-queues-ordered-unbounded.ad[bs]	\
bc-indefinite_containers-queues-ordered-unmanaged.ad[bs]	\
bc-indefinite_containers-queues-ordered.ad[bs]

ADA_SOURCE +=							\
bc-indefinite_unmanaged_containers-collections-ordered.ad[bs]	\
bc-indefinite_unmanaged_containers-collections.ad[bs]		\
bc-indefinite_unmanaged_containers-maps.ad[bs]			\
bc-indefinite_unmanaged_containers-queues.ad[bs]		\
bc-indefinite_unmanaged_containers.ad[bs]


############################
# Distribution construction

SOURCE = Makefile $(ADA_SOURCE)

dist:
	mkdir $(DIST)/src
	tar cf - $(SOURCE) | (cd $(DIST)/src; tar xvf -)


.PHONY: dist force

# vim: textwidth=0 nowrap tabstop=8 shiftwidth=4 softtabstop=4 noexpandtab
# vim: filetype=make encoding=utf-8 fileformat=unix
