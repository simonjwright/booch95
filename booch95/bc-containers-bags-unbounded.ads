--  Copyright (C) 1994-2002 Grady Booch and Simon Wright.
--  All Rights Reserved.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with BC.Support.Unbounded;
with BC.Support.Hash_Tables;
with System.Storage_Pools;

generic
   with function Hash (V : Item) return Natural is <>;
   Buckets : Positive;
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
package BC.Containers.Bags.Unbounded is

   pragma Elaborate_Body;

   --  A bag denotes a collection of items, drawn from some
   --  well-defined universe. A bag may contain duplicate items. A bag
   --  actually owns only one copy of each unique item: duplicates are
   --  counted, but are not stored with the bag.

   --  The hash function (the generic parameter Hash) determines the
   --  allocation of items to hash buckets. The value returned must
   --  not change during the lifetime of a given Item. The range of
   --  hash values need not be constrained to the number of buckets in
   --  the bag.

   --  The hash function must satisfy the condition that, for objects
   --  A and B, if A = B, then Hash (A) must equal Hash (B). The hash
   --  function should attempt to spread the set of possible items
   --  uniformly across the number of buckets. The quality of the hash
   --  function has a significant impact upon performance.

   type Unconstrained_Bag
     (Number_Of_Buckets : Positive) is new Abstract_Bag with private;

   subtype Bag is Unconstrained_Bag (Number_Of_Buckets => Buckets);

   function Null_Container return Unconstrained_Bag;
   --  Note, this function has to be provided but the object returned
   --  is in fact a Bag (ie, it is constrained).

   procedure Clear (B : in out Unconstrained_Bag);
   --  Empty the bag of all items.

   procedure Add (B : in out Unconstrained_Bag; I : Item; Added : out Boolean);
   --  Add the item to the bag. If the item is not already a distinct
   --  member of the bag, copy the item and add it to the bag and set
   --  Added to True. If the item already exists, then increment the
   --  number of that item and set Added to False.

   procedure Remove (B : in out Unconstrained_Bag; I : Item);
   --  If the item is not a member of the bag, raise
   --  BC.Not_Found. Otherwise, if there is exactly one of the item in
   --  the bag, remove the item in the bag; if there is more than one
   --  of the item in the bag, simply decrement its number.

   function Extent (B : Unconstrained_Bag) return Natural;
   --  Return the number of distinct items in the bag.

   function Count (B : Unconstrained_Bag; I : Item) return Natural;
   --  Return the number of times the item occurs in the bag.

   function Is_Empty (B : Unconstrained_Bag) return Boolean;
   --  Return True if and only if there are no items in the bag.

   function Is_Member (B : Unconstrained_Bag; I : Item) return Boolean;
   --  Return True if and only if the item exists in the bag.

   function New_Iterator
     (For_The_Bag : Unconstrained_Bag) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Bag.

private

   package IC is new BC.Support.Unbounded (Item => Item,
                                           Item_Ptr => Item_Ptr,
                                           Storage => Storage);
   use IC;
   package Items is new BC.Support.Hash_Tables.Item_Signature
     (Item => Item,
      Item_Container => IC.Unb_Node);

   type Positive_Ptr is access all Positive;
   package VC is new BC.Support.Unbounded (Item => Positive,
                                           Item_Ptr => Positive_Ptr,
                                           Storage => Storage);
   use VC;
   package Values is new BC.Support.Hash_Tables.Value_Signature
     (Value => Positive,
      Value_Ptr => Positive_Ptr,
      Value_Container => VC.Unb_Node);

   package Tables is new BC.Support.Hash_Tables.Tables
     (Items => Items,
      Values => Values);

   type Unconstrained_Bag
     (Number_Of_Buckets : Positive)
   is new Abstract_Bag with record
      Rep : Tables.Table (Number_Of_Buckets => Number_Of_Buckets);
   end record;

   procedure Attach (B : in out Unconstrained_Bag; I : Item; C : Positive);

   procedure Detach (B : in out Unconstrained_Bag; I : Item);

   procedure Set_Value (B : in out Unconstrained_Bag; I : Item; C : Positive);

   function Number_Of_Buckets (B : Unconstrained_Bag) return Natural;

   function Length (B : Unconstrained_Bag; Bucket : Positive) return Natural;

   function Item_At (B : Unconstrained_Bag;
                     Bucket, Index : Positive) return Item_Ptr;

   function Value_At (B : Unconstrained_Bag;
                      Bucket, Index : Positive) return Positive;

end BC.Containers.Bags.Unbounded;
