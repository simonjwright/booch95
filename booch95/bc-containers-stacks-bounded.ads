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
package BC.Containers.Stacks.Bounded is

  type Bounded_Stack is new Stack with private;

  procedure Clear (Obj : in out Bounded_Stack);
  -- Empty the Stack of all items.

  procedure Push (Obj : in out Bounded_Stack; Elem : Item);
  -- Add a copy of the item to the top of the Stack.

  procedure Pop (Obj : in out Bounded_Stack);
  -- Remove the item from the top of the Stack.

  function Available (Obj : in Bounded_Stack) return Natural;
  -- Returns a count of the number of empty "Item slots" left.

  function Depth (Obj : in Bounded_Stack) return Natural;
  -- Returns the number of items in the Stack

  function Is_Empty (Obj : in Bounded_Stack) return Boolean;
  -- Returns True if and only if no items are in the stack

  function Top (Obj : in Bounded_Stack) return Item;
  -- Return a copy of the item at the top of the Stack.

  -- XXX need accessor generic

  function "=" (Left, Right : in Bounded_Stack) return boolean;
  -- Return True if and only if both stacks have the same depth and the
  -- same items in the same order; return False otherwise.

  function New_Iterator (For_The_Stack : Bounded_Stack) return Iterator;
  -- Return a reset Iterator bound to the specific Stack.

private

  function Cardinality (Obj : Bounded_Stack) return Natural;
  procedure Purge (Obj : in out Bounded_Stack);
  procedure Add (Obj : in out Bounded_Stack; Elem : Item);
  function Item_At (Obj : Bounded_Stack; Index : Positive) return Item_Ptr;

  package Bounded_Stack_Nodes
  is new BC.Support.Bounded (Item => Item,
                             Item_Ptr => Item_Ptr,
                             Maximum_Size => Maximum_Size);
  use Bounded_Stack_Nodes;

  type Bounded_Stack is new Stack with record
    Rep : Bounded_Stack_Nodes.Bnd_Node_Ref := new Bnd_Node;
  end record;

  procedure Adjust (Obj : in out Bounded_Stack);
  procedure Finalize (Obj : in out Bounded_Stack);

end BC.Containers.Stacks.Bounded;
