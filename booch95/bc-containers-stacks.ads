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

generic
package BC.Containers.Stacks is

  type Stack is abstract new Container with private;

  -- A sequence in which items may be added from one end and removed from
  -- the opposite end.  This class is abstract and serves only to enforce
  -- the interfaces among classes.

  -- Operations of equality, inequality, and assignment are "deep" for
  -- all Stack forms

  procedure Clear (S : in out Stack) is abstract;
  -- Empty the Stack of all items.

  procedure Push (S : in out Stack; Elem : Item) is abstract;
  -- Add a copy of the item to the top of the Stack.

  procedure Pop (S : in out Stack) is abstract;
  -- Remove the item from the top of the Stack.

  function Depth (S : in Stack) return Natural is abstract;
  -- Returns the number of items in the Stack

  function Is_Empty (S : in Stack) return Boolean is abstract;
  -- Returns True if and only if no items are in the stack

  function Top (S : in Stack) return Item is abstract;
  -- Return a copy of the item at the top of the Stack.

  -- XXX We need a generic accessor here ..

  function Are_Equal (Left, Right : Stack'Class) return Boolean;
  -- Return True if and only if both stacks have the same depth and the
  -- same items in the same order; return False otherwise.

  procedure Copy (From : Stack'Class; To : in out Stack'Class);
  -- This operation MUST be called for dissimilar Stacks in place of
  -- assignment.

private

  type Stack is abstract new Container with null record;

  procedure Add (S : in out Stack; Elem : Item);
  procedure Remove (S : in out Stack; From : Positive);

  type Stack_Iterator (S : access Stack'Class)
  is new Actual_Iterator (S) with record
    Index : Natural;
  end record;

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Initialize (It : in out Stack_Iterator);

  procedure Reset (It : in out Stack_Iterator);

  procedure Next (It : in out Stack_Iterator);

  function Is_Done (It : Stack_Iterator) return Boolean;

  function Current_Item (It : Stack_Iterator) return Item;

  function Current_Item (It : Stack_Iterator) return Item_Ptr;

  procedure Delete_Item_At (It : Stack_Iterator);

end BC.Containers.Stacks;
