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

generic
package BC.Containers.Queues is

  pragma Elaborate_Body;

  type Queue is abstract new Container with private;

  -- A queue denotes a sequence of items, in which items may be added
  -- from one end and removed from the opposite end of the sequence.

  procedure Clear (Q : in out Queue) is abstract;
  -- Empty the queue of all items.

  procedure Append (Q : in out Queue; Elem : Item) is abstract;
  -- Add the item to the back of the queue; the item itself is copied.

  procedure Pop (Q : in out Queue) is abstract;
  -- Remove the item from the front of the queue.

  procedure Remove (Q : in out Queue; From : Positive) is abstract;
  -- Remove the item at the given index (may be a balking operation).

  function Length (Q : in Queue) return Natural is abstract;
  -- Return the number of items in the queue.

  function Is_Empty (Q : in Queue) return Boolean is abstract;
  -- Return True if and only if there are no items in the queue.

  function Front (Q : in Queue) return Item is abstract;
  -- Return a copy of the item at the front of the queue.

  generic
    with procedure Process (Elem : in out Item);
  procedure Process_Front (Q : in out Queue'Class);
  -- Access the item at the front of the queue.

  function Location (Q : in Queue; Elem : in Item) return Natural
    is abstract;
  -- Return the first index at which the item is found; return 0 if the
  -- item does not exist in the queue.

  function Are_Equal (Left, Right : Queue'Class) return Boolean;
  -- Return True if and only if both queues have the same length and the same
  -- items in the same order; return False otherwise.

  procedure Copy (From : Queue'Class; To : in out Queue'Class);
  -- This operation MUST be called for dissimilar Queues in place of
  -- assignment.

private

  type Queue is abstract new Container with null record;

  type Queue_Iterator is new Iterator with record
    Index : Natural;
  end record;

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Reset (It : in out Queue_Iterator);

  procedure Next (It : in out Queue_Iterator);

  function Is_Done (It : Queue_Iterator) return Boolean;

  function Current_Item (It : Queue_Iterator) return Item;

  function Current_Item_Ptr (It : Queue_Iterator) return Item_Ptr;

  procedure Delete_Item_At (It : Queue_Iterator);

end BC.Containers.Queues;
