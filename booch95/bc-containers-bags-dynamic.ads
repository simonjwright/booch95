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

with BC.Support.Dynamic;
with BC.Support.Hash_Tables;
with System.Storage_Pools;

generic
  with function Hash (V : Item) return Positive is <>;
  Buckets : Positive;
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Bags.Dynamic is

  -- A bag denotes a collection of items, drawn from some well-defined
  -- universe. A bag may contain duplicate items. A bag actually owns only
  -- one copy of each unique item: duplicates are counted, but are not
  -- stored with the bag.

  -- The hash function (the generic parameter Hash) determines the
  -- allocation of pairs to hash buckets. The value returned must not
  -- change during the lifetime of a given Item. The range of hash values
  -- need not be constrained to the number of buckets in the bag.

  -- The hash function must satisfy the condition that, for objects A and
  -- B, if A = B, then Hash (A) must equal Hash (B). The hash function
  -- should attempt to spread the set of possible items uniformly across
  -- the number of buckets. The quality of the hash function has a
  -- significant impact upon performance.

  type Dynamic_Bag is new Bag with private;

  function Create (Size : Positive) return Dynamic_Bag;
  -- Creates a new Dynamic Bag each of whose buckets is preallocated for
  -- 'Size' elements

  procedure Clear (B : in out Dynamic_Bag);
  -- Empty the bag of all items.

  procedure Add (B : in out Dynamic_Bag; I : Item; Added : out Boolean);
  -- Add the item to the bag. If the item is not already a distinct member
  -- of the bag, copy the item and add it to the bag and set Added to
  -- True. If the item already exists, then increment the number of that
  -- item and set Added to False.

  procedure Remove (B : in out Dynamic_Bag; I : Item);
  -- If the item is not a member of the bag, raise BC.Not_Found. Otherwise,
  -- if there is exactly one of the item in the bag, remove the item in the
  -- bag; if there is more than one of the item in the bag, simply decrement
  -- its number.

  function Extent (B : Dynamic_Bag) return Natural;
  -- Return the number of distinct items in the bag.

  function Count (B : Dynamic_Bag; I : Item) return Natural;
  -- Return the number of times the item occurs in the bag.

  function Is_Empty (B : Dynamic_Bag) return Boolean;
  -- Return True if and only if there are no items in the bag.

  function Is_Member (B : Dynamic_Bag; I : Item) return Boolean;
  -- Return True if and only if the item exists in the bag.

  procedure Preallocate (B : in out Dynamic_Bag; Size : Positive);
  -- Allocates 'Size' additional storage elements for each bucket of the
  -- Bag

  procedure Set_Chunk_Size (B : in out Dynamic_Bag; Size : Positive);
  -- Establishes the Size each bucket of the Bag will grow if the Bag
  -- exhausts its current size.

  function Chunk_Size (B : Dynamic_Bag) return Positive;
  -- Returns the Chunk_Size.

  function New_Iterator (For_The_Bag : Dynamic_Bag) return Iterator;
  -- Return a reset Iterator bound to the specific Bag.

private

  package IC is new BC.Support.Dynamic (Item => Item,
                                        Item_Ptr => Item_Ptr,
                                        Storage_Manager => Storage_Manager,
                                        Storage => Storage);
  use IC;

  type Positive_Ptr is access all Positive;
  package VC is new BC.Support.Dynamic (Item => Positive,
                                        Item_Ptr => Positive_Ptr,
                                        Storage_Manager => Storage_Manager,
                                        Storage => Storage);
  use VC;

  package Tables is new BC.Support.Hash_Tables
     (Item => Item,
      Value => Positive,
      Value_Ptr => Positive_Ptr,
      Buckets => Buckets,
      Item_Container => IC.Dyn_Node,
      Item_Container_Ptr => IC.Dyn_Node_Ref,
      Value_Container => VC.Dyn_Node,
      Value_Container_Ptr => VC.Dyn_Node_Ref);

  type Dynamic_Bag is new Bag with record
    Rep : Tables.Table;
  end record;

  procedure Purge (B : in out Dynamic_Bag);

  procedure Attach (B : in out Dynamic_Bag; I : Item; C : Positive);

  procedure Detach (B : in out Dynamic_Bag; I : Item);

  procedure Set_Value (B : in out Dynamic_Bag; I : Item; C : Positive);

  function Cardinality (B : Dynamic_Bag) return Natural;

  function Number_Of_Buckets (B : Dynamic_Bag) return Natural;

  function Length (B : Dynamic_Bag; Bucket : Positive) return Natural;

  function Exists (B : Dynamic_Bag; I : Item) return Boolean;

  function Item_At
     (B : Dynamic_Bag; Bucket, Index : Positive) return Item_Ptr;

  function Value_At
     (B : Dynamic_Bag; Bucket, Index : Positive) return Positive;

  type Dynamic_Bag_Iterator (U : access Dynamic_Bag'Class)
  is new Bag_Iterator (U) with null record;

end BC.Containers.Bags.Dynamic;
