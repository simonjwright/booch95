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
with BC.Support.Nodes;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Trees.Multiway is

  pragma Elaborate_Body;

  type Multiway_Tree is private;

  function Create (From : Multiway_Tree) return Multiway_Tree;
  -- If the given tree is null; construct a null tree. Otherwise, construct
  -- a tree that structurally shares the root of the given tree.

  function "=" (Left, Right : Multiway_Tree) return Boolean;
  -- Return True if and only if both trees are null or structurally share the
  -- same tree.

  procedure Clear (T : in out Multiway_Tree);
  -- If the tree is not null, destroy this alias to the tree, make the tree
  -- null, and reclaim the storage associated with any unreachable items.

  procedure Insert (T : in out Multiway_Tree; Elem : in Item);
  -- Add the item to the root of the tree and make the original root the
  -- immediate child of this new tree.

  procedure Append (T : in out Multiway_Tree;
                    Elem : in Item);
  -- Add the item as the immediate child of the tree.

  procedure Append (T : in out Multiway_Tree;
                    Elem : in Item;
                    After : Positive);
  -- Add the item as a child of the tree, after the given indexed child.

  procedure Append (T : in out Multiway_Tree;
                    From_Tree : in out Multiway_Tree);
  -- Add the tree as the immediate child of the tree.

  procedure Remove (T : in out Multiway_Tree; Index : Positive);
  -- Remove the given child and destroy it if it is no longer reachable.

  procedure Share (T : in out Multiway_Tree;
                   Share_With : in Multiway_Tree;
                   Child : Positive);
  -- Clear the tree, then, if the given tree is not null, set the tree to
  -- structurally share with the given child of the tree.

  procedure Swap_Child (T : in out Multiway_Tree;
                        Swap_WIth : in out Multiway_Tree;
                        Child : in Positive);
  -- The given tree must represent the root of a tree, which may be
  -- null. Set the child of the tree (which may be null) to denote the
  -- given tree (which may be null), and set the given tree to the original
  -- child of the tree. If it is not null, the parent of the new child of
  -- the tree is set to be the root of the tree.  If it is not null, the
  -- parent of the new root of the given tree is set to be null.

  procedure Child (T : in out Multiway_Tree; Child : in Positive);
  -- The tree must not be null. Set the tree to now denote the given child
  -- (which may be null) and reclaim the storage associated with any
  -- unreachable items.

  procedure Parent (T : in out Multiway_Tree);
  -- Set the tree to now denote its parent (if any).

  procedure Set_Item (T : in out Multiway_Tree; Elem : in Item);
  -- Set the item at the root of the tree.

  function Arity (T : Multiway_Tree) return Natural;
  -- Return the number of children relative to the root of the tree.

  function Has_Children (T : in Multiway_Tree) return Boolean;
  -- Return True if and only if the tree has any non-null children.

  function Is_Null (T : in Multiway_Tree) return Boolean;
  -- Return True if and only if the tree has no items.

  function Is_Shared (T : in Multiway_Tree) return Boolean;
  -- Return True if and only if the tree has an alias.

  function Is_Root (T : in Multiway_Tree) return Boolean;
  -- Return True if and only if the tree is at the root of a tree.

  function Item_At (T : in Multiway_Tree) return Item;
  -- Return the item at the root of the tree.

private

  package Nodes is new BC.Support.Nodes (Item, Storage_Manager, Storage);

  type Multiway_Tree is new Ada.Finalization.Controlled with record
    Rep : Nodes.Multiway_Node_Ref;
  end record;

  procedure Initialize (T : in out Multiway_Tree);
  procedure Adjust (T : in out Multiway_Tree);
  procedure Finalize (T : in out Multiway_Tree);

end BC.Containers.Trees.Multiway;
