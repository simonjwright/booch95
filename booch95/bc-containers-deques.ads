-- Copyright (C) 1994-2000 Grady Booch and Simon Wright.
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
package BC.Containers.Deques is

  pragma Elaborate_Body;

  type Deque_End is (Front, Back);
  
  type Deque is abstract new Container with private;

  -- A deque denotes a sequence of items, in which items may be added
  -- and removed from either end of the sequence.

  procedure Clear (D : in out Deque) is abstract;
  -- Empty the deque of all items.

  procedure Append (D : in out Deque;
		    Elem : Item;
		    Location : Deque_End := Back) is abstract;
  -- Add the item to the deque at the given location; the item itself
  -- is copied.

  procedure Pop (D : in out Deque; Location : Deque_End := Front) is abstract;
  -- Remove the item from the deque at the given location.

  procedure Remove (D : in out Deque; From : Positive) is abstract;
  -- Remove the item at the given index (may be a balking operation).

  function Length (D : in Deque) return Natural is abstract;
  -- Return the number of items in the deque.

  function Is_Empty (D : in Deque) return Boolean is abstract;
  -- Return True if and only if there are no items in the deque.

  function Front (D : in Deque) return Item is abstract;
  -- Return a copy of the item at the front of the deque.

  generic
    with procedure Process (Elem : in out Item);
  procedure Process_Front (D : in out Deque'Class);
  -- Access the item at the front of the deque.

  function Back (D : in Deque) return Item is abstract;
  -- Return a copy of the item at the back of the deque.

  generic
    with procedure Process (Elem : in out Item);
  procedure Process_Back (D : in out Deque'Class);
  -- Access the item at the back of the deque.

  function Location (D : in Deque; Elem : in Item) return Natural
    is abstract;
  -- Return the first index at which the item is found; return 0 if the
  -- item does not exist in the deque.

  function Are_Equal (Left, Right : Deque'Class) return Boolean;
  -- Return True if and only if both deques have the same length and the same
  -- items in the same order; return False otherwise.

  procedure Copy (From : Deque'Class; To : in out Deque'Class);
  -- This operation MUST be called for dissimilar Deques in place of
  -- assignment.

private

  type Deque is abstract new Container with null record;

  type Deque_Iterator (D : access Deque'Class)
  is new Actual_Iterator (D) with record
    Index : Natural;
  end record;

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Initialize (It : in out Deque_Iterator);

  procedure Reset (It : in out Deque_Iterator);

  procedure Next (It : in out Deque_Iterator);

  function Is_Done (It : Deque_Iterator) return Boolean;

  function Current_Item (It : Deque_Iterator) return Item;

  function Current_Item (It : Deque_Iterator) return Item_Ptr;

  procedure Delete_Item_At (It : Deque_Iterator);

end BC.Containers.Deques;
