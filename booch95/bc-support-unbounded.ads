-- Copyright (C) 1994-1999 Grady Booch, David Weller, Pat Rogers and
-- Simon Wright.
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

with BC.Support.Nodes;
with System.Storage_Pools;

generic
  type Item is private;
  with function "=" (L, R : Item) return Boolean is <>;
  type Item_Ptr is access all Item;
  type Storage_Manager(<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Support.Unbounded is

  type Unb_Node is private;
  -- An unpacked container whose items are stored on the heap.
  -- Items are effectively indexed from 1.

  type Unb_Node_Ref is access all Unb_Node;

  function Create (From : Unb_Node) return Unb_Node_Ref;
  -- Construct a new unbounded container that is identical to the given
  -- container

  function "=" (Left, Right : Unb_Node) return Boolean;

  procedure Clear (Obj : in out Unb_Node);
  -- Empty the container of all Items

  procedure Insert (Obj : in out Unb_Node; Elem : Item);
  -- Add an item to the front of the container

  procedure Insert (Obj : in out Unb_Node; Elem : Item; Before : Positive);
  -- Add an item to the container, before the given index

  procedure Append (Obj : in out Unb_Node; Elem : Item);
  -- Add an item to the end of the container

  procedure Append (Obj : in out Unb_Node; Elem : Item; After : Positive);
  -- Add an item to the container, after the given index

  procedure Remove (Obj : in out Unb_Node; From : Positive);
  -- Remove the item at the given index

  procedure Replace (Obj : in out Unb_Node; Index : Positive; Elem : Item);
  -- Replace the item at index with the new elem

  function Length (Obj : Unb_Node) return Natural;
  -- Returns the number of items in the container

  function First (Obj : Unb_Node) return Item;
  function First (Obj : Unb_Node) return Item_Ptr;
  -- Returns the item at the front of the container

  function Last (Obj : Unb_Node) return Item;
  function Last (Obj : Unb_Node) return Item_Ptr;
  -- Returns the item at the end of the container

  function Item_At (Obj : Unb_Node; Index : Positive) return Item;
  function Item_At (Obj : Unb_Node; Index : Positive) return Item_Ptr;
  -- Returns the item at the given index

  function Location (Obj : Unb_Node; Elem : Item; Start : Positive := 1)
                     return Natural;
  -- Returns the first index in which the given item is found. Returns 0
  -- if unsuccessful.

  procedure Free (Obj : in out Unb_Node_Ref);
  -- Dispose of the node referred to, having first cleared it

private

  package Nodes is new BC.Support.Nodes (Item, Storage_Manager, Storage);

  type Unb_Node is tagged record
    Rep : Nodes.Node_Ref;
    Last : Nodes.Node_Ref;
    Size : Natural := 0;
    Cache : Nodes.Node_Ref;
    Cache_Index : Natural := 0; -- 0 means invalid
  end record;
  -- We make this type tagged solely so that it's a "by-reference" type (we
  -- don't want a copy to be passed, we want the actual node).

end BC.Support.Unbounded;
