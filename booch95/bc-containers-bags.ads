-- Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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
package BC.Containers.Bags is

  pragma Elaborate_Body;

  -- A bag denotes a collection of items, drawn from some well-defined
  -- universe. A bag may contain duplicate items. A bag actually owns only
  -- one copy of each unique item: duplicates are counted, but are not
  -- stored with the bag.

  -- The parameter Item denotes the universe from which the bag draws its
  -- items. Items may be a primitive type or user-defined.

  type Abstract_Bag is abstract new Container with private;

  function Are_Equal (L, R : Abstract_Bag'Class) return Boolean;
  -- Return True if and only if both bags have the same number of distinct
  -- items, and the same items themselves, each with the same count; return
  -- False otherwise.
  -- Can't call this "=" because of the standard one for Bag.

  procedure Clear (B : in out Abstract_Bag) is abstract;
  -- Empty the bag of all items.

  procedure Add
     (B : in out Abstract_Bag; I : Item; Added : out Boolean) is abstract;
  -- Add the item to the bag. If the item is not already a distinct member
  -- of the bag, copy the item and add it to the bag and set Added to
  -- True. If the item already exists, then increment the number of that
  -- item and set Added to False.

  procedure Add (B : in out Abstract_Bag'Class; I : Item);
  -- Add the item to the bag. If the item is not already a distinct member
  -- of the bag, copy the item and add it to the bag; if it is, increment
  -- the number of that item.

  procedure Remove (B : in out Abstract_Bag; I : Item) is abstract;
  -- If the item is not a member of the bag, raise BC.Not_Found. Otherwise,
  -- if there is exactly one of the item in the bag, remove the item in the
  -- bag; if there is more than one of the item in the bag, simply decrement
  -- its number.

  procedure Union (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class);
  -- Perform a logical bag union; at the completion of this operation, the
  -- bag B contains the items and counts found in its original state
  -- combined with the bag O. For each item in the bag O, if the item is
  -- not already a distinct member of the bag S, copy the item and add it
  -- and its count to the bag S. If the item already is a member, increment
  -- its count in S.

  procedure Intersection
     (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class);
  -- Perform a logical bag intersection; at the completion of this
  -- operation, the bag B contains the items found both in its original
  -- state and in the bag O. For each item in the bag O, if the item is not
  -- already a distinct member of the bag S, do nothing. If the item
  -- already is a member of S, set its count to the lower of the two
  -- counts. Items in the bag S but not in the bag O are also removed.

  procedure Difference (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class);
  -- Perform a logical bag difference; at the completion of this operation,
  -- the bag B contains the items found in its original state, less those
  -- found in the bag O. For each item in the bag O, if the item is not
  -- already a distinct member of the bag S, do nothing. If the item is a
  -- member of the bag S with a count less than that in the bag O, remove
  -- the item from the bag S. If the item is a member of the bag S with a
  -- count more than that in the bag O, decrement the count in the bag S by
  -- the count in the bag O.

  function Extent (B : Abstract_Bag) return Natural is abstract;
  -- Return the number of distinct items in the bag.

  function Total_Size (B : Abstract_Bag'Class) return Natural;
  -- Return the total number of items in the bag.

  function Count (B : Abstract_Bag; I : Item) return Natural is abstract;
  -- Return the number of times the item occurs in the bag.

  function Is_Empty (B : Abstract_Bag) return Boolean is abstract;
  -- Return True if and only if there are no items in the bag.

  function Is_Member (B : Abstract_Bag; I : Item) return Boolean is abstract;
  -- Return True if and only if the item exists in the bag.

  function Is_Subset
     (B : Abstract_Bag'Class; O : Abstract_Bag'Class) return Boolean;
  -- Return True if and only if the bag B has the same or fewer distinct
  -- items than in the bag O and equal or less numbers of each such item
  -- than in the bag O.

  function Is_Proper_Subset
     (B : Abstract_Bag'Class; O : Abstract_Bag'Class) return Boolean;
  -- Return True if and only if
  -- all the distinct items in the bag B are also in the bag O, and
  -- either at least one of the items in the bag B has a lower number
  -- than the number in the bag O,
  -- or there is at least one distinct item in the bag O that is not
  -- in the bag B.

private

  type Abstract_Bag is abstract new Container with null record;

  procedure Attach (B : in out Abstract_Bag; I : Item; C : Positive);

  procedure Detach (B : in out Abstract_Bag; I : Item);

  procedure Set_Value (B : in out Abstract_Bag; I : Item; C : Positive);

  function Multiplicity (B : Abstract_Bag'Class) return Natural;

  function Number_Of_Buckets (B : Abstract_Bag) return Natural;

  function Length (B : Abstract_Bag; Bucket : Positive) return Natural;

  function Item_At
     (B : Abstract_Bag; Bucket, Index : Positive) return Item_Ptr;

  function Value_At
     (B : Abstract_Bag; Bucket, Index : Positive) return Positive;

  type Bag_Iterator is new Iterator with record
    Bucket_Index : Natural := 0;
    Index : Natural := 0;
  end record;

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Reset (It : in out Bag_Iterator);

  procedure Next (It : in out Bag_Iterator);

  function Is_Done (It : Bag_Iterator) return Boolean;

  function Current_Item (It : Bag_Iterator) return Item;

  function Current_Item_Ptr (It : Bag_Iterator) return Item_Ptr;

  procedure Delete_Item_At (It : Bag_Iterator);

end BC.Containers.Bags;
