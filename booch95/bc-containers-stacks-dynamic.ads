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

with BC.Support.Dynamic;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Stacks.Dynamic is

  pragma Elaborate_Body;

  type Dynamic_Stack is new Stack with private;
  -- A dynamic Stack exhibits similar performance to a Bounded_Stack,
  -- except its size is limited only to available memory.  It dynamically
  -- grows in a linear fashion (based on Chunk_Size).  There is currently
  -- no support for linear collapsing of the Stack.

  function Create (Size : Positive) return Dynamic_Stack;
  -- Creates a new Dynamic Stack that is preallocated for 'Size' elements

  function "=" (Left, Right : Dynamic_Stack) return Boolean;
  -- Return True if and only if both stacks have the same depth and the
  -- same items in the same order; return False otherwise.

  procedure Clear (S : in out Dynamic_Stack);
  -- Empty the Stack of all items.

  procedure Push (S : in out Dynamic_Stack; Elem : Item);
  -- Add a copy of the item to the top of the Stack.

  procedure Pop (S : in out Dynamic_Stack);
  -- Remove the item from the top of the Stack.

  function Depth (S : in Dynamic_Stack) return Natural;
  -- Returns the number of items in the Stack

  function Is_Empty (S : in Dynamic_Stack) return Boolean;
  -- Returns True if and only if no items are in the stack

  function Top (S : in Dynamic_Stack) return Item;
  -- Return a copy of the item at the top of the Stack.

  procedure Preallocate (S : in out Dynamic_Stack; Size : Natural);
  -- Allocates 'Size' additional storage elements for the Stack

  procedure Set_Chunk_Size (S : in out Dynamic_Stack; Size : Natural);
  -- Establishes the Size the Stack will grow if the Stack exhausts its
  -- current size.

  function Chunk_Size (S : Dynamic_Stack) return Natural;
  -- Returns the Chunk_Size

  function New_Iterator (For_The_Stack : Dynamic_Stack) return Iterator;
  -- Return a reset Iterator bound to the specific Stack.

private

  function Cardinality (S : Dynamic_Stack) return Natural;
  function Item_At (S : Dynamic_Stack; Index : Positive) return Item_Ptr;
  procedure Purge (S : in out Dynamic_Stack);

  procedure Add (S : in out Dynamic_Stack; Elem : Item);
  procedure Remove (S : in out Dynamic_Stack; From : Positive);

  package Dynamic_Stack_Nodes
  is new BC.Support.Dynamic (Item => Item,
                             Item_Ptr => Item_Ptr,
                             Storage_Manager => Storage_Manager,
                             Storage => Storage);

  type Dynamic_Stack is new Stack with record
    Rep : Dynamic_Stack_Nodes.Dyn_Node_Ref;
  end record;

  procedure Initialize (S : in out Dynamic_Stack);
  procedure Adjust (S : in out Dynamic_Stack);
  procedure Finalize (S : in out Dynamic_Stack);

end BC.Containers.Stacks.Dynamic;
