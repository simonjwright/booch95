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

with Ada.Finalization;
with Bc.Support.Nodes;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Trees.Binary is

  type Binary_Tree is new Ada.Finalization.Controlled with private;

  type Child_Branch is (Left, Right);

  function Create (From : Binary_Tree) return Binary_Tree;
  -- If the given tree is null; construct a null tree. Otherwise, construct
  -- a tree that structurally shares the root of the given tree.

  function "=" (Left, Right : Binary_Tree) return Boolean;
  -- Return True if and only if both trees are null or structurally share the
  -- same tree.

  procedure Clear (Obj : in out Binary_Tree);
  -- If the tree is not null, destroy this alias to the tree, make the tree
  -- null, and reclaim the storage associated with any unreachable items.

  procedure Insert (Obj : in out Binary_Tree;
                    Elem : in Item;
                    Child : in Child_Branch);
  -- Add the item to the root of the tree, and make the original root the
  -- given child of this new tree.

  procedure Append (Obj : in out Binary_Tree;
                    Elem : in Item;
                    Child : in Child_Branch;
                    After : in Child_Branch);
  -- Add the item as the given child of the tree, and take the original
  -- child and add it as the child after this new item.

  procedure Remove (Obj : in out Binary_Tree; Child : in Child_Branch);
  -- Remove the given child and destroy it if it is no longer reachable.

  procedure Share (Obj : in out Binary_Tree;
                   Share_With : in Binary_Tree;
                   Child : in Child_Branch);
  -- Clear the tree, then, if the given tree is not null, set the tree to
  -- structurally share with the given child of the tree.

  procedure Swap_Child (Obj : in out Binary_Tree;
                        Swap_WIth : in out Binary_Tree;
                        Child : in Child_Branch);
  -- The given tree must represent the root of a tree, which may be
  -- null. Set the child of the tree (which may be null) to denote the
  -- given tree (which may be null), and set the given tree to the original
  -- child of the tree. If it is not null, the parent of the new child of
  -- the tree is set to be the root of the tree.  If it is not null, the
  -- parent of the new root of the given tree is set to be null.

  procedure Child (Obj : in out Binary_Tree; Child : in Child_Branch);
  -- The tree must not be null. Set the tree to now denote the given child
  -- (which may be null) and reclaim the storage associated with any
  -- unreachable items.

  procedure Left_Child (Obj : in out Binary_Tree);
  -- The tree must not be null. Set the tree to now denote the left child
  -- (which may be null) and reclaim the storage associated with any
  -- unreachable items.

  procedure Right_Child (Obj : in out Binary_Tree);
  -- The tree must not be null. Set the tree to now denote the right child
  -- (which may be null) and reclaim the storage associated with any
  -- unreachable items.

  procedure Parent (Obj : in out Binary_Tree);
  -- Set the tree to now denote its parent (if any).

  procedure Set_Item (Obj : in out Binary_Tree; Elem : in Item);
  -- Set the item at the root of the tree.

  function Has_Children (Obj : in Binary_Tree) return Boolean;
  -- Return True if and only if the tree has any non-null children.

  function Is_Null (Obj : in Binary_Tree) return Boolean;
  -- Return True if and only if the tree has no items.

  function Is_Shared (Obj : in Binary_Tree) return Boolean;
  -- Return True if and only if the tree has an alias.

  function Is_Root (Obj : in Binary_Tree) return Boolean;
  -- Return True if and only if the tree is at the root of a tree.

  function Item_At (Obj : in Binary_Tree) return Item;
  -- Return the item at the root of the tree.

private

  package Nodes is new Bc.Support.Nodes (Item, Storage_Manager, Storage);

  procedure Purge (Node : in out Nodes.Binary_Node_Ref);

  type Binary_Tree is new Ada.Finalization.Controlled with record
    Rep : Nodes.Binary_Node_Ref;
  end record;

  procedure Initialize (Obj : in out Binary_Tree);
  procedure Adjust (Obj : in out Binary_Tree);
  procedure Finalize (Obj : in out Binary_Tree);

end BC.Containers.Trees.Binary;
