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
  type Item is private;
  type Item_Ptr is access all Item;
  Maximum_Size : Positive;
package BC.Support.Bounded is

  Max_Size : constant Positive := Maximum_Size;

  type Bnd_Node is private;
  -- An optimally-packed static container whose items are stored on the
  -- stack.  Items are indexable starting at 1.  This is a supporting
  -- type.  As such, it is not intended to be used directly by the
  -- end-user.

  subtype Size_Range is Natural range 0..Max_Size;

  type Bnd_Node_Ref is access all Bnd_Node;

  function Create (Obj : in Bnd_Node) return Bnd_Node_Ref;

  -- Creation, Equality, and Inequality predefined

  procedure Clear (Obj : in out Bnd_Node);
  -- Empty the container of all Items

  procedure Insert (Obj : in out Bnd_Node; Elem : Item);
  -- Add an item to the front of the container

  procedure Insert (Obj : in out Bnd_Node; Elem : Item; Before : Natural);
  -- Add an item to the container, before the given index

  procedure Append (Obj : in out Bnd_Node; Elem : Item);
  -- Add an item to the end of the container

  procedure Append (Obj : in out Bnd_Node; Elem : Item; After : Natural);
  -- Add an item to the end of the container, after the given index

  procedure Remove (Obj : in out Bnd_Node; From : Natural);
  -- Remove the item at a given index

  procedure Replace (Obj : in out Bnd_Node; Index : Natural; Elem : Item);
  -- Replace the Item at Index with the new Elem

  function Available (Obj: Bnd_Node) return Natural;
  -- Returns available storage elements

  function Length (Obj : Bnd_Node) return Natural;
  -- Returns the number of items in the container

  function First (Obj : Bnd_Node) return Item;
  function First (Obj : Bnd_Node) return Item_Ptr;
  -- Returns the Item at the front of the container

  function Last (Obj : Bnd_Node) return Item;
  function Last (Obj : Bnd_Node) return Item_Ptr;
  -- Returns the item at the end of the container

  function Item_At (Obj : Bnd_Node; Index : Positive) return Item;
  function Item_At (Obj : Bnd_Node; Index : Positive) return Item_Ptr;
  -- Returns the item at the given index

  function Location (Obj : Bnd_Node;
                     Elem : in Item;
                     Start : in Natural := 1) return Natural;
  -- Returns the first index in which the given item is found. Returns 0
  -- if unsuccessful.

  procedure Free (Obj : in out Bnd_Node_Ref);
  -- Dispose of the Node referred to, having first Cleared it

private

  type Elem_Array is array (Positive range 1 .. Max_Size) of aliased Item;

  -- XXX
  -- this representation is not the same as the C++ components
  -- why is Elems aliased? -- so we can take 'Access of components
  type Bnd_Node is record
    Elems : aliased Elem_Array;
    Size : Size_Range := 0;
  end record;

end BC.Support.Bounded;
