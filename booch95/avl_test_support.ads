-- Copyright (C) 1999, 2001 Simon Wright.
-- All Rights Reserved.
--
-- This program is distributed in the hope that it will be
--      useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--      PURPOSE. See the Ada Community License for more details.
--      You should have received a copy of the Ada Community
--      License with this library, in the file named "Ada Community
--      License" or "ACL". If not, contact the author of this library
--      for a copy.
--

-- $Id$

with BC.Containers;
with BC.Containers.Trees;
with BC.Containers.Trees.AVL;
with BC.Containers.Trees.AVL.Print;
with BC.Containers.Trees.AVL.Validate;
with Global_Heap;

package AVL_Test_Support is

  type Key is range 0 .. 1023;

  type Item is record
    K : Key;
    Count : Integer := 0;
  end record;

  function "=" (L, R : Item) return Boolean;
  function "<" (L, R : Item) return Boolean;
  function Image (Val : Item) return String;

  package Containers is new BC.Containers (Item => Item);

  package Trees is new Containers.Trees;

  package TA is new Trees.AVL (Storage_Manager => Global_Heap.Pool,
                               Storage => Global_Heap.Storage);

  procedure Print is new TA.Print (Image => Image);

  procedure Validate is new TA.Validate;

end AVL_Test_Support;
