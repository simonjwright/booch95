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

with BC.Support.Unbounded;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Rings.Unbounded is

  pragma Elaborate_Body;

  type Unbounded_Ring is new Ring with private;

  function "=" (Left, Right : in Unbounded_Ring) return Boolean;

  procedure Clear (R : in out Unbounded_Ring);
  -- Empty the ring of all items. The mark is cleared.

  procedure Insert (R : in out Unbounded_Ring; Elem : Item);
  -- If the ring was empty, set the ring's mark and top to designate
  -- this item.
  -- Otherwise,
  --   this item becomes the new top;
  --   the previous top is located one place forward of the new top;
  --   the mark remains on the previously marked item.

  procedure Pop (R : in out Unbounded_Ring);
  -- Remove the top item from the ring.
  -- If the ring is still not empty, the new top is the item that was
  -- previously one place forward from the top.
  -- If the removed item was the marked item, the mark now designates
  -- the new top.

  procedure Rotate (R : in out Unbounded_Ring; Dir : Direction := Forward);
  -- Rotate the top of the ring in the given direction. The ring's
  -- mark is unaffected. If there is exactly one item in the ring,
  -- rotating either direction always returns to the same item.

  function Extent (R : Unbounded_Ring) return Natural;
  -- Return the number of items in the ring.

  function Is_Empty (R : Unbounded_Ring) return Boolean;
  -- Return True if and only if there are no items in the ring.

  function Top (R : Unbounded_Ring) return Item;
  -- Return a copy of the item at the top of the ring.

  function New_Iterator (For_The_Ring : Unbounded_Ring) return Iterator'Class;
  -- Return a reset Iterator bound to the specific Ring.

private

  procedure Add (R : in out Unbounded_Ring; Elem : Item);
  function Item_At (R : Unbounded_Ring; Index : Positive) return Item_Ptr;

  package Unbounded_Ring_Nodes
  is new BC.Support.Unbounded (Item => Item,
                               Item_Ptr => Item_Ptr,
                               Storage_Manager => Storage_Manager,
                               Storage => Storage);

  type Unbounded_Ring is new Ring with record
    Rep : Unbounded_Ring_Nodes.Unb_Node_Ref
       := new Unbounded_Ring_Nodes.Unb_Node;
  end record;

  procedure Initialize (R : in out Unbounded_Ring);
  procedure Adjust (R : in out Unbounded_Ring);
  procedure Finalize (R : in out Unbounded_Ring);

end BC.Containers.Rings.Unbounded;
