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
package BC.Containers.Stacks.Unbounded is

  type Unbounded_Stack is new Stack with private;
  -- This Stack exhibits unlimited growth and collapsing, limited only by
  -- available memory.  Assignment is "deep".

  function "=" (Left, Right : in Unbounded_Stack) return boolean;
  -- Return True if and only if both stacks have the same depth and the
  -- same items in the same order; return False otherwise.

  procedure Clear (Obj : in out Unbounded_Stack);
  -- Empty the Stack of all items.

  procedure Push (Obj : in out Unbounded_Stack; Elem : Item);
  -- Add a copy of the item to the top of the Stack.

  procedure Pop (Obj : in out Unbounded_Stack);
  -- Remove the item from the top of the Stack.

  function Depth (Obj : in Unbounded_Stack) return Natural;
  -- Returns the number of items in the Stack

  function Is_Empty (Obj : in Unbounded_Stack) return Boolean;
  -- Returns True if and only if no items are in the stack

  function Top (Obj : in Unbounded_Stack) return Item;
  -- Return a copy of the item at the top of the Stack.

  -- XXX need accessor generic

  function New_Iterator (For_The_Stack : Unbounded_Stack) return Iterator;
  -- Return a reset Iterator bound to the specific Stack.

private

  function Item_At (Obj : Unbounded_Stack; Index : Positive) return Item_Ptr;
  function Cardinality (Obj : Unbounded_Stack) return Natural;
  procedure Purge (Obj : in out Unbounded_Stack);
  procedure Add (Obj : in out Unbounded_Stack; Elem : Item);

  package Unbounded_Stack_Nodes
  is new BC.Support.Unbounded (Item => Item,
                               Item_Ptr => Item_Ptr,
                               Storage_Manager => Storage_Manager,
                               Storage => Storage);

  type Unbounded_Stack is new Stack with record
    Rep : Unbounded_Stack_Nodes.Unb_Node_Ref
       := new Unbounded_Stack_Nodes.Unb_Node;
  end record;

  procedure Initialize (Obj : in out Unbounded_Stack);
  procedure Adjust (Obj : in out Unbounded_Stack);
  procedure Finalize (Obj : in out Unbounded_Stack);

end BC.Containers.Stacks.Unbounded;
