-- Copyright (C) 1994-1998 Grady Booch, David Weller, Pat Rogers and
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

with Ada.Finalization;
with BC.Support.Nodes;
with System.Storage_Pools;

generic
  type Item is private;
  type Item_Ptr is access all Item;
  type Storage_Manager(<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Support.Dynamic is

  type Dyn_Node is private;
  -- An optimally-packed dynamic container whose items are stored on the heap

  type Dyn_Node_Ref is access all Dyn_Node;

  function Create (From : Dyn_Node) return Dyn_Node_Ref;
  -- Construct a new dynamic container that is identical to the given container

  function Create (Size : Positive := 10) return Dyn_Node_Ref;
  -- Construct a new dynamic container that has a chunk_Size set to the
  -- passed-in value

  function "=" (Left, Right : Dyn_Node) return Boolean;
  -- Creation, Equality, and Inequality predefined

  procedure Clear (Obj : in out Dyn_Node);
  -- Empty the container of all Items

  procedure Insert (Obj : in out Dyn_Node; Elem : Item);
  -- Add an item to the front of the container

  procedure Insert (Obj : in out Dyn_Node; Elem : Item; Before : Positive);
  -- Add an item to the container, before the given index

  procedure Append (Obj : in out Dyn_Node; Elem : Item);
  -- Add an item to the end of the container

  procedure Append (Obj : in out Dyn_Node; Elem : Item; After : Positive);
  -- Add an item to the end of the container, after the given index

  procedure Remove (Obj : in out Dyn_Node; From : Positive);
  -- Remove the item at a given index

  procedure Replace (Obj : in out Dyn_Node; Index : Positive; Elem : Item);
  -- Replace the Item at Index with the new Elem

  function Length (Obj : Dyn_Node) return Natural;
  -- Returns the number of items in the container

  function First (Obj : Dyn_Node) return Item;
  function First (Obj : Dyn_Node) return Item_Ptr;
  -- Returns the Item at the front of the container

  function Last (Obj : Dyn_Node) return Item;
  function Last (Obj : Dyn_Node) return Item_Ptr;
  -- Returns the item at the end of the container

  function Item_At (Obj : Dyn_Node; Index : Positive) return Item;
  function Item_At (Obj : Dyn_Node; Index : Positive) return Item_Ptr;
  -- Returns the item at the given index

  function Location (Obj : Dyn_Node; Elem : Item; Start : Positive := 1)
                     return Natural;
  -- Returns the first index in which the given item is found. Returns 0
  -- if unsuccessful.

  procedure Preallocate (Obj : in out Dyn_Node; New_Length : Natural := 10);
  -- Preallocate New_Length number of unused items for the container

  procedure Set_Chunk_Size (Obj: in out Dyn_Node; Size : Natural);
  -- Set the Chunk_Size for the container

  function Chunk_Size (Obj : in Dyn_Node) return Natural;
  -- Returns the current Chunk_Size

  procedure Free (Obj : in out Dyn_Node_Ref);
  -- Dispose of the Node referred to, having first Cleared it

private

  type Dyn_Arr is array (Positive range <>) of aliased Item;

  type Dyn_Arr_Ref is access all Dyn_Arr;
  for Dyn_Arr_Ref'Storage_Pool use Storage;

  type Dyn_Node is new Ada.Finalization.Controlled with record
    Ref        : Dyn_Arr_Ref;
    Size       : Natural := 0;
    Chunk_Size : Natural := 10;
  end record;

  for Dyn_Node_Ref'Storage_Pool use Storage;

  procedure Initialize (D : in out Dyn_Node);
  procedure Adjust (D : in out Dyn_Node);
  procedure Finalize (D : in out Dyn_Node);

end BC.Support.Dynamic;
