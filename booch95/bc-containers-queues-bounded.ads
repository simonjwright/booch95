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

with BC.Support.Bounded;

generic
  Maximum_Size : Positive;
package BC.Containers.Queues.Bounded is

  type Bnd_Queue is new Queue with private;

  procedure Clear (Obj : in out Bnd_Queue);
  -- Empty the queue of all items.

  procedure Append (Obj : in out Bnd_Queue; Elem : Item);
  -- Add the item to the back of the queue; the item itself is copied.

  procedure Pop (Obj : in out Bnd_Queue);
  -- Remove the item from the front of the queue.

  procedure Remove (Obj : in out Bnd_Queue; From : Natural);
  -- Remove the item at the given index (may be a balking operation).

  function Available (Obj : in Bnd_Queue) return Natural;
  -- Indicated number of empty "Item slots" left in Queue

  function Length (Obj : in Bnd_Queue) return Natural;
  -- Remove the item at the given index (may be a balking operation).

  function Is_Empty (Obj : in Bnd_Queue) return Boolean;
  -- Return True if and only if there are no items in the queue.

  function Front (Obj : in Bnd_Queue) return Item;
  -- Return a copy of the item at the front of the queue.

  function Front (Obj : in Bnd_Queue) return Item_Ptr;
  -- Return a pointer to the item at the front of the queue.

  function Location (Obj : in Bnd_Queue; Elem : Item) return Natural;
  -- Return the first index at which the item is found; return 0 if the
  -- item does not exist in the queue.

  function "=" (Left, Right : in Bnd_Queue) return boolean;
  -- Return True if and only if both queues have the same length and the same
  -- items in the same order; return False otherwise.

private

  function Cardinality (Obj : in Bnd_Queue) return Integer;
  function Item_At (Obj : in Bnd_Queue; Index : Natural) return Item_Ptr;
  procedure Purge (Obj : in out Bnd_Queue);
  procedure Add (Obj : in out Bnd_Queue; Elem : in out Item);

  package Bnd_Queue_Nodes
  is new BC.Support.Bounded (Item,Item_Ptr,Maximum_Size);

  type Bnd_Queue is new Queue with record
    Rep : Bnd_Queue_Nodes.Bnd_Node_Ref := new Bnd_Queue_Nodes.Bnd_Node;
  end record;

  procedure Adjust (Obj : in out Bnd_Queue);
  procedure Finalize (Obj : in out Bnd_Queue);

end BC.Containers.Queues.Bounded;
