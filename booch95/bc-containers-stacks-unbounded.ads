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
package BC.Containers.Stacks.Unbounded is

  type Unb_Stack is new Stack with private;
  -- This Stack exhibits unlimited growth and collapsing, limited only by
  -- available memory.  Assignment is "deep".

  function "=" (Left, Right : in Unb_Stack) return boolean;
  -- Return True if and only if both stacks have the same depth and the
  -- same items in the same order; return False otherwise.

  procedure Clear (Obj : in out Unb_Stack);
  -- Empty the Stack of all items.

  procedure Push (Obj : in out Unb_Stack; Elem : Item);
  -- Add a copy of the item to the top of the Stack.

  procedure Pop (Obj : in out Unb_Stack);
  -- Remove the item from the top of the Stack.

  function Depth (Obj : in Unb_Stack) return Natural;
  -- Returns the number of items in the Stack

  function Is_Empty (Obj : in Unb_Stack) return Boolean;
  -- Returns True if and only if no items are in the stack

  function Top (Obj : in Unb_Stack) return Item;
  -- Return a copy of the item at the top of the Stack.

  function Top (Obj : in Unb_Stack) return Item_Ptr;
  -- Return a pointer to the item at the top of the Stack.

private

  function Item_At (Obj : in Unb_Stack; Index : in Natural) return Item_Ptr;
  function Cardinality (Obj : in Unb_Stack) return Integer;
  procedure Purge (Obj : in out Unb_Stack);
  procedure Add (Obj : in out Unb_Stack; Elem : in out Item);

  package Unb_Stack_Nodes
  is new BC.Support.Unbounded (Item, Item_Ptr, Storage_Manager, Storage);

  type Unb_Stack is new Stack with record
    Rep : Unb_Stack_Nodes.Unb_Node_Ref := new Unb_Stack_Nodes.Unb_Node;
  end record;

  procedure Initialize (Obj : in out Unb_Stack);
  procedure Adjust (Obj : in out Unb_Stack);
  procedure Finalize (Obj : in out Unb_Stack);

end BC.Containers.Stacks.Unbounded;
