-- Copyright (C) 1994-2000 Grady Booch, David Weller and Simon Wright.
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
package BC.Containers.Deques.Unbounded is

  pragma Elaborate_Body;

  type Unbounded_Deque is new Deque with private;
  -- This Deque exhibits unlimited growth and collapsing, limited only by
  -- available memory.  Assignment is "deep".

  procedure Clear (D : in out Unbounded_Deque);
  -- Empty the deque of all items.

  procedure Append (D : in out Unbounded_Deque; 
		    Elem : Item;
		    Location : Deque_End := Back);
  -- Add the item to the deque at the given location; the item itself
  -- is copied.

  procedure Pop (D : in out Unbounded_Deque; Location : Deque_End := Front);
  -- Remove the item from the deque at the given location.

  procedure Remove (D : in out Unbounded_Deque; From : Positive);
  -- Remove the item at the given index.

  function Length (D : in Unbounded_Deque) return Natural;
  -- Return the number of items in the deque.

  function Is_Empty (D : in Unbounded_Deque) return Boolean;
  -- Return True if and only if there are no items in the deque.

  function Front (D : in Unbounded_Deque) return Item;
  -- Return a copy of the item at the front of the deque.

  function Back (D : in Unbounded_Deque) return Item;
  -- Return a copy of the item at the back of the deque.

  function Location (D : in Unbounded_Deque; Elem : Item) return Natural;
  -- Return the first index at which the item is found; return 0 if the
  -- item does not exist in the deque.

  function "=" (Left, Right : in Unbounded_Deque) return Boolean;
  -- Return True if and only if both deques have the same length and the same
  -- items in the same order; return False otherwise.

  function New_Iterator (For_The_Deque : Unbounded_Deque) return Iterator;
  -- Return a reset Iterator bound to the specific Deque.

private

  package Unbounded_Deque_Nodes
  is new BC.Support.Unbounded (Item => Item,
                               Item_Ptr => Item_Ptr,
                               Storage_Manager => Storage_Manager,
                               Storage => Storage);

  type Unbounded_Deque is new Deque with record
    Rep : Unbounded_Deque_Nodes.Unb_Node_Ref
       := new Unbounded_Deque_Nodes.Unb_Node;
  end record;

  function Item_At (D : Unbounded_Deque; Index : Positive) return Item_Ptr;

  procedure Initialize (D : in out Unbounded_Deque);
  procedure Adjust (D : in out Unbounded_Deque);
  procedure Finalize (D : in out Unbounded_Deque);

end BC.Containers.Deques.Unbounded;
