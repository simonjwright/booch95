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

with BC.Support.Unbounded;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Queues.Unbounded is

  type Unbounded_Queue is new Queue with private;
  -- This Queue exhibits unlimited growth and collapsing, limited only by
  -- available memory.  Assignment is "deep".

  procedure Clear (Obj : in out Unbounded_Queue);
  -- Empty the queue of all items.

  procedure Append (Obj : in out Unbounded_Queue; Elem : Item);
  -- Add the item to the back of the queue; the item itself is copied.

  procedure Pop (Obj : in out Unbounded_Queue);
  -- Remove the item from the front of the queue.

  procedure Remove (Obj : in out Unbounded_Queue; From : Positive);
  -- Remove the item at the given index.

  function Length (Obj : in Unbounded_Queue) return Natural;
  -- Return the number of items in the queue.

  function Is_Empty (Obj : in Unbounded_Queue) return Boolean;
  -- Return True if and only if there are no items in the queue.

  function Front (Obj : in Unbounded_Queue) return Item;
  -- Return a copy of the item at the front of the queue.

  -- XXX need accessor generic
  function Location (Obj : in Unbounded_Queue; Elem : Item) return Natural;
  -- Return the first index at which the item is found; return 0 if the
  -- item does not exist in the queue.

  function "=" (Left, Right : in Unbounded_Queue) return Boolean;
  -- Return True if and only if both queues have the same length and the same
  -- items in the same order; return False otherwise.

  function New_Iterator (For_The_Queue : Unbounded_Queue) return Iterator;
  -- Return a reset Iterator bound to the specific Queue.

private

  package Unbounded_Queue_Nodes
  is new BC.Support.Unbounded (Item => Item,
                               Item_Ptr => Item_Ptr,
                               Storage_Manager => Storage_Manager,
                               Storage => Storage);

  type Unbounded_Queue is new Queue with record
    Rep : Unbounded_Queue_Nodes.Unb_Node_Ref
       := new Unbounded_Queue_Nodes.Unb_Node;
  end record;

  function Item_At (Obj : Unbounded_Queue; Index : Positive) return Item_Ptr;
  function Cardinality (Obj : Unbounded_Queue) return Natural;
  procedure Purge (Obj : in out Unbounded_Queue);
  procedure Add (Obj : in out Unbounded_Queue; Elem : Item);

  procedure Initialize (Obj : in out Unbounded_Queue);
  procedure Adjust (Obj : in out Unbounded_Queue);
  procedure Finalize (Obj : in out Unbounded_Queue);

end BC.Containers.Queues.Unbounded;
