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

with BC.Support.Bounded_Hash_Tables;

generic
   with function Hash (V : Item) return Natural is <>;
   Buckets : Positive;
   Maximum_Size : Positive;
package BC.Containers.Bags.Bounded is

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

   type Bag is new Abstract_Bag with private;

   function Null_Container return Bag;

   procedure Clear (B : in out Bag);
   --  Empty the bag of all items.

   procedure Add (B : in out Bag; I : Item; Added : out Boolean);
   --  Add the item to the bag. If the item is not already a distinct
   --  member of the bag, copy the item and add it to the bag and set
   --  Added to True. If the item already exists, then increment the
   --  number of that item and set Added to False.

   procedure Remove (B : in out Bag; I : Item);
   --  If the item is not a member of the bag, raise
   --  BC.Not_Found. Otherwise, if there is exactly one of the item in
   --  the bag, remove the item in the bag; if there is more than one
   --  of the item in the bag, simply decrement its number.

   function Available (B : Bag) return Natural;
   --  Return the number of unused slots in the bag. Note, since hash
   --  buckets are of fixed size in the bounded bag it will probably
   --  not be possible to use all these slots.

   function Extent (B : Bag) return Natural;
   --  Return the number of distinct items in the bag.

   function Count (B : Bag; I : Item) return Natural;
   --  Return the number of times the item occurs in the bag.

   function Is_Empty (B : Bag) return Boolean;
   --  Return True if and only if there are no items in the bag.

   function Is_Member (B : Bag; I : Item) return Boolean;
   --  Return True if and only if the item exists in the bag.

   function New_Iterator (For_The_Bag : Bag) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Bag.

private

   package Items is new BC.Support.Bounded_Hash_Tables.Item_Signature
     (Item => Item,
      Item_Ptr => Item_Ptr,
      Eq => "=");

   type Positive_Ptr is access all Positive;
   package Values is new BC.Support.Bounded_Hash_Tables.Value_Signature
     (Value => Positive,
      Value_Ptr => Positive_Ptr,
      Eq => "=");

   package Tables is new BC.Support.Bounded_Hash_Tables.Tables
     (Items => Items,
      Values => Values,
      Buckets => Buckets,
      Maximum_Size => Maximum_Size);

   type Bag is new Abstract_Bag with record
      Rep : Tables.Table;
   end record;

   procedure Attach (B : in out Bag; I : Item; C : Positive);

   procedure Detach (B : in out Bag; I : Item);

   procedure Set_Value (B : in out Bag; I : Item; C : Positive);

   function Number_Of_Buckets (B : Bag) return Natural;

   function Item_At (B : Bag; Bucket, Index : Positive) return Item_Ptr;

   function Value_At (B : Bag; Bucket, Index : Positive) return Positive;

   type Bounded_Bag_Iterator is new Bag_Iterator with null record;

   procedure Reset (It : in out Bounded_Bag_Iterator);

   procedure Next (It : in out Bounded_Bag_Iterator);

   function Is_Done (It : Bounded_Bag_Iterator) return Boolean;

   function Current_Item (It : Bounded_Bag_Iterator) return Item;

   function Current_Item_Ptr (It : Bounded_Bag_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Bounded_Bag_Iterator);

end BC.Containers.Bags.Bounded;
