-- Copyright (C) 1994-1999 Grady Booch, David Weller and Simon Wright.
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

with Ada.Finalization;
with BC.Support.Nodes;
with System.Storage_Pools;

generic
  with function "<" (L, R : Item) return Boolean is <>;
  with function "=" (L, R : Item) return Boolean is <>;
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Trees.AVL is

  type AVL_Tree is limited private;

  procedure Clear (Obj : in out AVL_Tree);
  -- Make the tree null and reclaim the storage associated with its items.

  procedure Insert (Obj : in out AVL_Tree;
                    Element : Item;
                    Not_Found : out Boolean);
  -- Add the item to the tree, preserving the tree's balance. Not_Found is
  -- set to True if the item had not previously existed in the tree, and
  -- to False otherwise.

  procedure Delete
     (Obj : in out AVL_Tree; Element : Item; Found : out Boolean);
  -- Remove the item from the tree, preserving the tree's balance. Found is
  -- set to True if the item was in fact found in the tree and removed, and
  -- to False otherwise.

  function Extent (Obj : AVL_Tree) return Natural;
  -- Return the number of items in the tree.

  function Is_Null (Obj : AVL_Tree) return Boolean;
  -- Return True if and only if the tree has no items.

  function Is_Member (Obj : AVL_Tree; Element : Item) return Boolean;
  -- Return True if and only if the item exists in the tree.

  generic
    with procedure Apply (Elem : in out Item);
  procedure Access_Actual_Item (In_The_Tree : AVL_Tree;
                                Elem : Item;
                                Found : out Boolean);
  -- If an Item "=" to Elem is present in the Tree, call Apply for it and
  -- set Found to True; otherwise, set Found to False.
  -- Apply MUST NOT alter the result of the ordering operation "<".

  generic
    with procedure Apply (Elem : in Item; OK : out Boolean);
  procedure Visit (Over_The_Tree : AVL_Tree);
  -- Call Apply with a copy of each Item in the Tree, in order. The
  -- iteration will terminate early if Apply sets OK to False.

  generic
    with procedure Apply (Elem : in out Item; OK : out Boolean);
  procedure Modify (Over_The_Tree : AVL_Tree);
  -- Call Apply for each Item in the Tree, in order. The iteration will
  -- terminate early if Apply sets OK to False.
  -- Apply MUST NOT alter the result of the ordering operation "<".

private

  package Nodes is new BC.Support.Nodes (Item, Storage_Manager, Storage);

  type AVL_Tree is new Ada.Finalization.Limited_Controlled with record
    Rep : Nodes.AVL_Node_Ref;
    Size : Natural := 0;
  end record;

  procedure Initialize (Obj : in out AVL_Tree);

  procedure Finalize (Obj : in out AVL_Tree);

end BC.Containers.Trees.AVL;
