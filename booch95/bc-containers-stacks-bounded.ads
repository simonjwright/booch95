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
package BC.Containers.Stacks.Bounded is

  type Bnd_Stack is new Stack with private;

  procedure Clear (Obj : in out Bnd_Stack);
  -- Empty the Stack of all items.

  procedure Push (Obj : in out Bnd_Stack; Elem : Item);
  -- Add a copy of the item to the top of the Stack.

  procedure Pop (Obj : in out Bnd_Stack);
  -- Remove the item from the top of the Stack.

  function Available (Obj : in Bnd_Stack) return Natural;
  -- Returns a count of the number of empty "Item slots" left.

  function Depth (Obj : in Bnd_Stack) return Natural;
  -- Returns the number of items in the Stack

  function Is_Empty (Obj : in Bnd_Stack) return Boolean;
  -- Returns True if and only if no items are in the stack

  function Top (Obj : in Bnd_Stack) return Item;
  -- Return a copy of the item at the top of the Stack.

  function Top (Obj : in Bnd_Stack) return Item_Ptr;
  -- Return a pointer to the item at the top of the Stack.

  function "=" (Left, Right : in Bnd_Stack) return boolean;
  -- Return True if and only if both stacks have the same depth and the
  -- same items in the same order; return False otherwise.

private

  function Cardinality (Obj : in Bnd_Stack) return Integer;
  procedure Purge (Obj : in out Bnd_Stack);
  procedure Add (Obj : in out Bnd_Stack; Elem : in out Item);
  function Item_At (Obj : in Bnd_Stack; Index : Natural) return Item_Ptr;

  package Bnd_Stack_Nodes
  is new BC.Support.Bounded(Item,Item_Ptr,Maximum_Size);
  use Bnd_Stack_Nodes;

  type Bnd_Stack is new Stack with record
    Rep : Bnd_Stack_Nodes.Bnd_Node_Ref := new Bnd_Node;
  end record;

  procedure Adjust (Obj : in out Bnd_Stack);
  procedure Finalize (Obj : in out Bnd_Stack);

end BC.Containers.Stacks.Bounded;
