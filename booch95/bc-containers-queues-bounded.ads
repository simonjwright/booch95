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

with BC.Support.Bounded;

generic
  Maximum_Size : Positive;
package BC.Containers.Queues.Bounded is

  type Bounded_Queue is new Queue with private;

  procedure Clear (Q : in out Bounded_Queue);
  -- Empty the queue of all items.

  procedure Append (Q : in out Bounded_Queue; Elem : Item);
  -- Add the item to the back of the queue; the item itself is copied.

  procedure Pop (Q : in out Bounded_Queue);
  -- Remove the item from the front of the queue.

  procedure Remove (Q : in out Bounded_Queue; From : Positive);
  -- Remove the item at the given index (may be a balking operation).

  function Available (Q : in Bounded_Queue) return Natural;
  -- Indicated number of empty "Item slots" left in Queue

  function Length (Q : in Bounded_Queue) return Natural;
  -- Remove the item at the given index (may be a balking operation).

  function Is_Empty (Q : in Bounded_Queue) return Boolean;
  -- Return True if and only if there are no items in the queue.

  function Front (Q : in Bounded_Queue) return Item;
  -- Return a copy of the item at the front of the queue.

  -- XXX need accessor generic

  function Location (Q : in Bounded_Queue; Elem : Item) return Natural;
  -- Return the first index at which the item is found; return 0 if the
  -- item does not exist in the queue.

  function "=" (Left, Right : in Bounded_Queue) return boolean;
  -- Return True if and only if both queues have the same length and the same
  -- items in the same order; return False otherwise.

  function New_Iterator (For_The_Queue : Bounded_Queue) return Iterator;
  -- Return a reset Iterator bound to the specific Queue.

private

  function Cardinality (Q : Bounded_Queue) return Natural;
  function Item_At (Q : Bounded_Queue; Index : Positive) return Item_Ptr;
  procedure Purge (Q : in out Bounded_Queue);

  package Bounded_Queue_Nodes
  is new BC.Support.Bounded (Item => Item,
                             Item_Ptr => Item_Ptr,
                             Maximum_Size => Maximum_Size);

  type Bounded_Queue is new Queue with record
    Rep : Bounded_Queue_Nodes.Bnd_Node_Ref := new Bounded_Queue_Nodes.Bnd_Node;
  end record;

  procedure Adjust (Q : in out Bounded_Queue);
  procedure Finalize (Q : in out Bounded_Queue);

end BC.Containers.Queues.Bounded;
