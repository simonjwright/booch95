-- Copyright (C) 2001 Simon Wright.
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

-- Implements a Quicksort of the given container in place, using the
-- supplied comparator.
--
-- The instantiating Container must fully support the operation
--
--   function Item_At (C : Container; Index : Positive) return Item_Ptr;
--
-- which is normally private and whose default implementation raises
-- Should_Have_Been_Overridden. If it's not fully supported, as is the
-- case for hash-table-based containers (Bags, Maps, Sets),
-- Container_Error will be raised.

generic
  with function "<" (L, R : Item) return Boolean is <>;
  type Container is new Containers.Container with private;
  with function Length (C : Container) return Natural is <>;
procedure BC.Containers.Quicksort (C : in out Container);
