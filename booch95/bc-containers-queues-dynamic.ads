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

with BC.Support.Dynamic;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Queues.Dynamic is

  type Dyn_Queue is new Queue with private;
  -- A dynamic Queue exhibits similar performance to a Bounded_Queue,
  -- except its size is limited only to available memory.  It dynamically
  -- grows in a linear fashion (based on Chunk_Size).  There is currently
  -- no support for linear collapsing of the Queue.

  function Create (Size : Positive) return Dyn_Queue;
  -- Creates a new Dynamic Queue that is preallocated for 'Size' elements

  procedure Clear (Obj : in out Dyn_Queue);
  -- Empty the queue of all items.

  procedure Append (Obj : in out Dyn_Queue; Elem : Item);
  -- Add the item to the back of the queue; the item itself is copied.

  procedure Pop (Obj : in out Dyn_Queue);
  -- Remove the item from the front of the queue.

  procedure Remove (Obj : in out Dyn_Queue; From : Natural);
  -- Remove the item at the given index.

  function Length (Obj : in Dyn_Queue) return Natural;
  -- Return the number of items in the queue.

  function Is_Empty (Obj : in Dyn_Queue) return Boolean;
  -- Return True if and only if there are no items in the queue.

  function Front (Obj : in Dyn_Queue) return Item;
  -- Return a copy of the item at the front of the queue.

  function Front (Obj : in Dyn_Queue) return Item_Ptr;
  -- Return a pointer to the item at the front of the queue.

  function Location (Obj : in Dyn_Queue; Elem : Item) return Natural;
  -- Return the first index at which the item is found; return 0 if the
  -- item does not exist in the queue.

  function "=" (Left, Right : Dyn_Queue) return Boolean;
  -- Return True if and only if both queues have the same length and the same
  -- items in the same order; return False otherwise.

  procedure Preallocate (Obj : in out Dyn_Queue; Size : Natural);
  -- Allocates 'Size' additional storage elements for the Queue

  procedure Set_Chunk_Size (Obj : in out Dyn_Queue; Size : Natural);
  -- Establishes the Size the Queue will grow if the Queue exhausts its
  -- current size.

  function Chunk_Size (Obj : Dyn_Queue) return Natural;
  -- Returns the Chunk_Size

private

  function Item_At (Obj : in Dyn_Queue; Index : in Natural) return Item_Ptr;
  function Cardinality (Obj : Dyn_Queue) return Integer;
  procedure Purge (Obj : in out Dyn_Queue);
  procedure Add (Obj : in out Dyn_Queue; Elem : in out Item);

  package Dyn_Queue_Nodes
  is new BC.Support.Dynamic (Item, Item_Ptr, Storage_Manager, Storage);

  type Dyn_Queue is new Queue with record
    Rep : Dyn_Queue_Nodes.Dyn_Node_Ref;
  end record;

  procedure Initialize (Obj : in out Dyn_Queue);
  procedure Adjust (Obj : in out Dyn_Queue);
  procedure Finalize (Obj : in out Dyn_Queue);

end BC.Containers.Queues.Dynamic;
