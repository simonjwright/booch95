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

with BC.Support.Unbounded;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Queues.Unbounded is

  type Unb_Queue is new Queue with private;
  -- This Queue exhibits unlimited growth and collapsing, limited only by
  -- available memory.  Assignment is "deep".

  procedure Clear (Obj : in out Unb_Queue);
  -- Empty the queue of all items.

  procedure Append (Obj : in out Unb_Queue; Elem : Item);
  -- Add the item to the back of the queue; the item itself is copied.

  procedure Pop (Obj : in out Unb_Queue);
  -- Remove the item from the front of the queue.

  procedure Remove (Obj : in out Unb_Queue; From : Natural);
  -- Remove the item at the given index.

  function Length (Obj : in Unb_Queue) return Natural;
  -- Return the number of items in the queue.

  function Is_Empty (Obj : in Unb_Queue) return Boolean;
  -- Return True if and only if there are no items in the queue.

  function Front (Obj : in Unb_Queue) return Item;
  -- Return a copy of the item at the front of the queue.

  function Front (Obj : in Unb_Queue) return Item_Ptr;
  -- Return a pointer to the item at the front of the queue.

  function Location (Obj : in Unb_Queue; Elem : Item) return Natural;
  -- Return the first index at which the item is found; return 0 if the
  -- item does not exist in the queue.

  function "=" (Left, Right : in Unb_Queue) return Boolean;
  -- Return True if and only if both queues have the same length and the same
  -- items in the same order; return False otherwise.

private

  package Unb_Queue_Nodes
  is new BC.Support.Unbounded (Item, Item_Ptr, Storage_Manager, Storage);

  type Unb_Queue is new Queue with record
    Rep : Unb_Queue_Nodes.Unb_Node_Ref := new Unb_Queue_Nodes.Unb_Node;
  end record;

  function Item_At (Obj : in Unb_Queue; Index : in Natural) return Item_Ptr;
  function Cardinality (Obj : in Unb_Queue) return Integer;
  procedure Purge (Obj : in out Unb_Queue);
  procedure Add (Obj : in out Unb_Queue; Elem : in out Item);

  procedure Initialize (Obj : in out Unb_Queue);
  procedure Adjust (Obj : in out Unb_Queue);
  procedure Finalize (Obj : in out Unb_Queue);

end BC.Containers.Queues.Unbounded;
