-- Copyright (C) 1994-1998 Grady Booch, David Weller and Simon Wright.
-- All Rights Reserved.
--
--      This program is free software; you can redistribute it
--      and/or modify it under the terms of the Ada Community
--      License which comes with this Library.
--
--      This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

-- $Id$

generic package BC.Containers.Trees is

  -- A binary tree is a rooted collection of nodes and arcs, where each
  -- node has two children and where arcs may not have cycles or
  -- cross-references (as in graphs). A multiway tree is a rooted
  -- collection of nodes and arcs, where each node may have an arbitrary
  -- number of children and where arcs may not have cycles or
  -- cross-references. AVL trees are a form of balance tree (following the
  -- algorithm of Adelson-Velskii and Landis) whose behavior only exposes
  -- operations to insert, delete, and search for items.

  -- Binary and multiway trees are polylithic structures, and hence the
  -- semantics of copying, assignment, and equality involve structural
  -- sharing. Care must be taken in manipulating the same tree named by
  -- more than one alias. AVL trees are monolithic, although they prohibit
  -- any copying, assignment, or test for equality.

  -- These classes are not intended to be subclassed, and so provide no
  -- virtual members.

  -- These abstractions have been carefully constructed to eliminate all
  -- storage leaks, except in the case of intentional abuses. When a tree
  -- is manipulated, all items that become unreachable are automatically
  -- reclaimed. Furthermore, this design protects against dangling
  -- references: an item is never reclaimed if there exists a reference to
  -- it.

  -- Unreachable items are those that belong to a tree or a subtree whose
  -- root is not designated by any alias. For example, consider the tree (A
  -- (B C (D E))), with the root of the tree designated by T1. T1 initially
  -- points to the root of the tree, at item A. Invoking the operation
  -- Right_Child on T1 now causes T1 to point to item C. Because A is now
  -- considered unreachable, the storage associated with item A is
  -- reclaimed; the parent of C is now null. Additionally, the sibling
  -- subtree rooted at B is also now unreachable, and so is reclaimed
  -- (along with its children, and recursively so). Similarly, consider the
  -- same tree, with the root of the tree designated by both T1 and
  -- T2. Both T1 and T2 are aliases that initially point to the root of the
  -- tree at item A. Invoking the operation Right_Child on T1 now causes T1
  -- to point to item C; T2 is unaffected. No storage is reclaimed, since
  -- every element of the tree is still reachable. Suppose we now invoke
  -- the member function Clear on T2. The semantics of this operation are
  -- such that only unreachable items are reclaimed. Thus, the storage
  -- associated with item A is reclaimed, because it is no longer
  -- reachable; additionally, the sibling B (and recursively so, its
  -- children) is reclaimed, because it is also now unreachable; the
  -- subtree denoted by T1 is unaffected. T2 is now null, and the parent of
  -- C is now null.

  -- It is possible, but not generally desirable, to produce multi-headed
  -- trees. In such cases, the parent of the item at the neck of a
  -- multi-headed tree points to the most recently attached root.

  -- The binary and multiway trees have a similar protocol, except that the
  -- binary tree adds two operations, Left_Child and Right_Child, and the
  -- multiway tree overloads the Append operation and adds the operation
  -- Arity. The AVL tree has a completely different protocol, with a much
  -- more limited set of operations.

end;
