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
package BC.Containers.Stacks.Dynamic is

  type Dyn_Stack is new Stack with private;
  -- A dynamic Stack exhibits similar performance to a Bounded_Stack,
  -- except its size is limited only to available memory.  It dynamically
  -- grows in a linear fashion (based on Chunk_Size).  There is currently
  -- no support for linear collapsing of the Stack.

  function Create (Size : Positive) return Dyn_Stack;
  -- Creates a new Dynamic Stack that is preallocated for 'Size' elements

  function "=" (Left, Right : Dyn_Stack) return Boolean;
  -- Return True if and only if both stacks have the same depth and the
  -- same items in the same order; return False otherwise.

  procedure Clear (Obj : in out Dyn_Stack);
  -- Empty the Stack of all items.

  procedure Push (Obj : in out Dyn_Stack; Elem : Item);
  -- Add a copy of the item to the top of the Stack.

  procedure Pop (Obj : in out Dyn_Stack);
  -- Remove the item from the top of the Stack.

  function Depth (Obj : in Dyn_Stack) return Natural;
  -- Returns the number of items in the Stack

  function Is_Empty (Obj : in Dyn_Stack) return Boolean;
  -- Returns True if and only if no items are in the stack

  function Top (Obj : in Dyn_Stack) return Item;
  -- Return a copy of the item at the top of the Stack.

  function Top (Obj : in Dyn_Stack) return Item_Ptr;
  -- Return a pointer to the item at the top of the Stack.

  procedure Preallocate (Obj : in out Dyn_Stack; Size : Natural);
  -- Allocates 'Size' additional storage elements for the Stack

  procedure Set_Chunk_Size (Obj : in out Dyn_Stack; Size : Natural);
  -- Establishes the Size the Stack will grow if the Stack exhausts its
  -- current size.

  function Chunk_Size (Obj : Dyn_Stack) return Natural;
  -- Returns the Chunk_Size

private
  function Item_At (Obj : in Dyn_Stack; Index : in Natural) return Item_Ptr;
  procedure Purge (Obj : in out Dyn_Stack);
  procedure Add (Obj : in out Dyn_Stack; Elem : in out Item);
  function Cardinality (Obj : Dyn_Stack) return Integer;

  package Dyn_Stack_Nodes
  is new BC.Support.Dynamic (Item, Item_Ptr, Storage_Manager, Storage);

  type Dyn_Stack is new Stack with record
    Rep : Dyn_Stack_Nodes.Dyn_Node_Ref;
  end record;

  procedure Initialize (Obj : in out Dyn_Stack);
  procedure Adjust (Obj : in out Dyn_Stack);
  procedure Finalize (Obj : in out Dyn_Stack);

end BC.Containers.Stacks.Dynamic;
