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

with BC.Support.Bounded;
with BC.Support.Hash_Tables;

generic
  with function Hash (V : Item) return Positive is <>;
  Buckets : Positive;
  Size : Positive;
package BC.Containers.Sets.Bounded is

  pragma Elaborate_Body;

  -- A set denotes a collection of items, drawn from some well-defined
  -- universe. A set may not contain duplicate items.

  -- The hash function (the generic parameter Hash) determines the
  -- allocation of pairs to hash buckets. The value returned must not
  -- change during the lifetime of a given Item. The range of hash values
  -- need not be constrained to the number of buckets in the set.

  -- The hash function must satisfy the condition that, for objects A and
  -- B, if A = B, then Hash (A) must equal Hash (B). The hash function
  -- should attempt to spread the set of possible items uniformly across
  -- the number of buckets. The quality of the hash function has a
  -- significant impact upon performance.

  type Bounded_Set is new Set with private;

  procedure Clear (S : in out Bounded_Set);
  -- Empty the set of all items.

  procedure Add (S : in out Bounded_Set; I : Item; Added : out Boolean);
  -- Add the item to the set. If the item is not already a distinct member
  -- of the set, copy the item and add it to the set and set Added to
  -- True. If the item already exists, then set Added to False.

  procedure Remove (S : in out Bounded_Set; I : Item);
  -- If the item is not a member of the set, raise BC.Not_Found. Otherwise,
  -- remove the item from the set.

  function Available (S : Bounded_Set) return Natural;
  -- Return the number of unused slots in the set. Note, since hash buckets
  -- are of fixed size in the bounded set it will probably not be possible
  -- to use all these slots.

  function Extent (S : Bounded_Set) return Natural;
  -- Return the number of distinct items in the set.

  function Is_Empty (S : Bounded_Set) return Boolean;
  -- Return True if and only if there are no items in the set.

  function Is_Member (S : Bounded_Set; I : Item) return Boolean;
  -- Return True if and only if the item exists in the set.

  function New_Iterator (For_The_Set : Bounded_Set) return Iterator;
  -- Return a reset Iterator bound to the specific Set.

private

  package IC is new BC.Support.Bounded (Item => Item,
                                        Item_Ptr => Item_Ptr,
                                        Maximum_Size => Size);
  use IC;
  package Items is new BC.Support.Hash_Tables.Item_Signature
     (Item => Item,
      Item_Container => IC.Bnd_Node,
      Item_Container_Ptr => IC.Bnd_Node_Ref);

  -- We need a dummy type for the Value component of the hash table.
  type Boolean_Ptr is access all Boolean;
  package VC is new BC.Support.Bounded (Item => Boolean,
                                        Item_Ptr => Boolean_Ptr,
                                        Maximum_Size => Size);
  use VC;
  package Values is new BC.Support.Hash_Tables.Value_Signature
     (Value => Boolean,
      Value_Ptr => Boolean_Ptr,
      Value_Container => VC.Bnd_Node,
      Value_Container_Ptr => VC.Bnd_Node_Ref);

  package Tables is new BC.Support.Hash_Tables.Tables
     (Items => Items,
      Values => Values,
      Buckets => Buckets);

  type Bounded_Set is new Set with record
    Rep : Tables.Table;
  end record;

  procedure Attach (S : in out Bounded_Set; I : Item);

  procedure Detach (S : in out Bounded_Set; I : Item);

  function Number_Of_Buckets (S : Bounded_Set) return Natural;

  function Length (S : Bounded_Set; Bucket : Positive) return Natural;

  function Item_At
     (S : Bounded_Set; Bucket, Index : Positive) return Item_Ptr;

  type Bounded_Set_Iterator (B : access Bounded_Set'Class)
  is new Set_Iterator (B) with null record;

end BC.Containers.Sets.Bounded;
