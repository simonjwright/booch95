-- Copyright (C) 1994-1999 Grady Booch and Simon Wright.
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
package BC.Containers.Rings is

  type Ring is abstract new Container with private;

  -- A ring denotes a sequence in which items may be added and removed
  -- from the top of a circular structure. Since this structure has no
  -- beginning or ending, a client can mark one particular item to
  -- designate a point of reference in the structure.

  -- XXX I am considerably confused by the use of "forward",
  -- "backward", "clockwise" etc; some clarification wouldn't come
  -- amiss!

  type Direction is (Forward, Backward); -- XXX why not clockwise, anticlockwise?

  function Are_Equal (Left, Right : Ring'Class) return Boolean;
  -- Return True if and only if both rings have the same extent and the
  -- same items in the same order; return False otherwise. The
  -- identity of the top and mark of both rings does not participate
  -- in this test of equality.
  -- Can't call this "=" because of the standard one for Ring.

  procedure Copy (From : Ring'Class; To : in out Ring'Class);
  -- This operation MUST be called for dissimilar Rings in place of
  -- assignment.

  procedure Clear (R : in out Ring) is abstract;
  -- Empty the ring of all items. The mark is cleared.

  procedure Insert (R : in out Ring; Elem : Item) is abstract;
  -- If the ring is empty, set the ring's mark to designate this
  -- item. Add the item to the top of the ring; the previous top item
  -- (if any) is now located clockwise adjacent to the new item; the
  -- old item is forward of the new one.

  procedure Pop (R : in out Ring) is abstract;
  -- Remove the item from the top of the ring. The clockwise adjacent
  -- item (if any) is now designated as the ring's top. If the removed
  -- item had been marked, the ring's new top (if not empty) is now
  -- designated as marked.

  procedure Rotate (R : in out Ring; Dir : Direction := Forward) is abstract;
  -- Rotate the top of the ring in the given direction. Rotating the
  -- ring in a forward direction moves the ring's top clockwise;
  -- rotating the ring in a reverse direction advances the ring's top
  -- counter-clockwise. The ring's mark is unaffected. If there is
  -- exactly one item in the ring, rotating either direction always
  -- returns to the same item.

  procedure Mark (R : in out Ring);
  -- Designate the item at the top of the ring (if not empty) as
  -- marked.

  procedure Rotate_To_Mark (R : in out Ring);
  -- Rotate the ring so that the ring's mark is at the top.

  function Extent (R : Ring) return Natural is abstract;
  -- Return the number of items in the ring.

  function Is_Empty (R : Ring) return Boolean is abstract;
  -- Return True if and only if there are no items in the ring.

  function Top (R : Ring) return Item is abstract;
  -- Return a copy of the item at the top of the ring.

  function At_Mark (R : Ring) return Boolean;
  -- Return True if and only if the item at the top of the ring is
  -- marked; otherwise, return False. By implication, this member function
  -- will return True if the ring is empty, since the ring's top and mark
  -- both do not designate any item.
  -- XXX hmm, odd logic there!

private

  type Ring is abstract new Container with record
    Top : Natural;      -- 0 implies not set
    Mark : Natural;     -- 0 implies not set
  end record;

  procedure Initialize (R : in out Ring); -- derivations will need to call this

  procedure Add (R : in out Ring; Elem : Item);

  procedure Lock (R : in out Ring);

  procedure Unlock (R : in out Ring);

  type Ring_Iterator (R : access Ring'Class)
  is new Actual_Iterator (R) with record
    Index : Natural;
  end record;

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Initialize (It : in out Ring_Iterator);

  procedure Reset (It : in out Ring_Iterator);

  procedure Next (It : in out Ring_Iterator);

  function Is_Done (It : Ring_Iterator) return Boolean;

  function Current_Item (It : Ring_Iterator) return Item;

  function Current_Item (It : Ring_Iterator) return Item_Ptr;

end BC.Containers.Rings;
