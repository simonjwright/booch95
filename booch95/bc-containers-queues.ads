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

generic
package Bc.Containers.Queues is

  type Queue is abstract new Container with private;

  -- A sequence in which items may be added from one end and removed from
  -- the opposite end.  This class is abstract and serves only to enforce
  -- the interfaces among classes.

  -- Operations of equality, inequality, and assignment are "deep" for
  -- all Queue forms

  procedure Clear   (Obj : in out Queue) is abstract;
  -- Empty the queue of all items.

  procedure Append  (Obj : in out Queue; Elem : Item) is abstract;
  -- Add the item to the back of the queue; the item itself is copied.

  procedure Pop     (Obj : in out Queue) is abstract;
  -- Remove the item from the front of the queue.

  procedure Remove  (Obj : in out Queue; From : Natural) is abstract;
  -- Remove the item at the given index (may be a balking operation).

  function Length   (Obj : in Queue) return Natural is abstract;
  -- Return the number of items in the queue.

  function Is_Empty (Obj : in Queue) return Boolean is abstract;
  -- Return True if and only if there are no items in the queue.

  function Front    (Obj : in Queue) return Item is abstract;
  -- Return a copy of the item at the front of the queue.

  function Front    (Obj : in Queue) return Item_Ptr is abstract;
  -- Return a pointer to the item at the front of the queue.

  function Location (Obj : in Queue; Elem : in Item) return Natural
    is abstract;
  -- Return the first index at which the item is found; return 0 if the
  -- item does not exist in the queue.

  function "=" (Left, Right : access Queue'Class) return Boolean;
  -- Return True if and only if both queues have the same length and the same
  -- items in the same order; return False otherwise.

  procedure Copy (From : access Queue'Class; To : access Queue'Class);
  -- This operation MUST be called for dissimilar Queues in place of
  -- assignment.

private

  type Queue is abstract new Container with null record;

  procedure Purge (Obj : in out Queue);
  procedure Add (Obj : in out Queue; Elem : in out Item);

end Bc.Containers.Queues;


