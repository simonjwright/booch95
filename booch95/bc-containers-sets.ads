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
package BC.Containers.Sets is

  pragma Elaborate_Body;

  -- A set denotes a collection of items, drawn from some well-defined
  -- universe. A set may not contain duplicate items.

  -- The parameter Item denotes the universe from which the set draws its
  -- items. Items may be a primitive type or user-defined.

  type Set is abstract new Container with private;

  function Are_Equal (L, R : Set'Class) return Boolean;
  -- Return True if and only if both sets have the same number of distinct
  -- items, and the same items themselves; return False otherwise.
  -- Can't call this "=" because of the standard one for Set.

  procedure Clear (S : in out Set) is abstract;
  -- Empty the set of all items.

  procedure Add (S : in out Set; I : Item; Added : out Boolean) is abstract;
  -- Add the item to the set. If the item is not already a distinct member
  -- of the set, copy the item and add it to the set and set Added to
  -- True. If the item already exists, then set Added to False.

  procedure Add (S : in out Set'Class; I : Item);
  -- Add the item to the set. If the item is not already a distinct member
  -- of the set, copy the item and add it to the set.

  procedure Remove (S : in out Set; I : Item) is abstract;
  -- If the item is not a member of the set, raise BC.Not_Found. Otherwise,
  -- remove the item from the set.

  procedure Union (S : in out Set'Class; O : Set'Class);
  -- Perform a logical set union; at the completion of this operation, the
  -- set S contains the items found in its original state combined with the
  -- set O (but without duplication). For each item in the set O, if the
  -- item is not already a distinct member of the set S, copy the item and
  -- add it to the set S. If the item already is a member, do nothing.

  procedure Intersection (S : in out Set'Class; O : Set'Class);
  -- Perform a logical set intersection; at the completion of this
  -- operation, the set S contains the items found both in its original
  -- state and in the set O. For each item in the set O, if the item is not
  -- already a distinct member of the set S, do nothing. If the item
  -- already is a member of S, do nothing. Items in the set S but not in
  -- the set O are also removed.

  procedure Difference (S : in out Set'Class; O : Set'Class);
  -- Perform a logical set difference; at the completion of this operation,
  -- the set S contains the items found in its original state, less those
  -- found in the set O. For each item in the set O, if the item is not
  -- already a distinct member of the set S, do nothing. If the item
  -- is a member, remove the item from the set S.

  function Extent (S : Set) return Natural is abstract;
  -- Return the number of distinct items in the set.

  function Is_Empty (S : Set) return Boolean is abstract;
  -- Return True if and only if there are no items in the set.

  function Is_Member (S : Set; I : Item) return Boolean is abstract;
  -- Return True if and only if the item exists in the set.

  function Is_Subset (S : Set'Class; O : Set'Class) return Boolean;
  -- Return True if and only if all the items in the set S are also in the
  -- set O.

  function Is_Proper_Subset (S : Set'Class; O : Set'Class) return Boolean;
  -- Return True if and only if all the items in the set S are also in the
  -- set O, and there is at least one item in O that is not in S.

private

  type Set is abstract new Container with null record;

  procedure Attach (S : in out Set; I : Item);

  procedure Detach (S : in out Set; I : Item);

  function Number_Of_Buckets (S : Set) return Natural;

  function Length (S : Set; Bucket : Positive) return Natural;

  function Exists (S : Set; I : Item) return Boolean;

  function Item_At (S : Set; Bucket, Index : Positive) return Item_Ptr;

  type Set_Iterator (S : access Set'Class)
  is new Actual_Iterator (S) with record
    Bucket_Index : Natural := 0;
    Index : Natural := 0;
  end record;

  procedure Initialize (It : in out Set_Iterator);

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Reset (It : in out Set_Iterator);

  procedure Next (It : in out Set_Iterator);

  function Is_Done (It : Set_Iterator) return Boolean;

  function Current_Item (It : Set_Iterator) return Item;

  function Current_Item (It : Set_Iterator) return Item_Ptr;

  procedure Delete_Item_At (It : Set_Iterator);

end BC.Containers.Sets;
