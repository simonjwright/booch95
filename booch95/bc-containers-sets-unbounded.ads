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
package BC.Containers.Sets.Unbounded is

   pragma Elaborate_Body;

   --  A set denotes a collection of items, drawn from some
   --  well-defined universe. A set may not contain duplicate items.

   --  The hash function (the generic parameter Hash) determines the
   --  allocation of items to hash buckets. The value returned must
   --  not change during the lifetime of a given Item. The range of
   --  hash values need not be constrained to the number of buckets in
   --  the set.

   --  The hash function must satisfy the condition that, for objects
   --  A and B, if A = B, then Hash (A) must equal Hash (B). The hash
   --  function should attempt to spread the set of possible items
   --  uniformly across the number of buckets. The quality of the hash
   --  function has a significant impact upon performance.

   type Unconstrained_Set
     (Number_Of_Buckets : Positive) is new Abstract_Set with private;

   subtype Set is Unconstrained_Set (Number_Of_Buckets => Buckets);

   function Null_Container return Unconstrained_Set;
   --  Note, this function has to be provided but the object returned
   --  is in fact a Set (ie, it is constrained).

   procedure Clear (S : in out Unconstrained_Set);
   --  Empty the set of all items.

   procedure Add (S : in out Unconstrained_Set; I : Item; Added : out Boolean);
   --  Add the item to the set. If the item is not already a distinct
   --  member of the set, copy the item and add it to the set and set
   --  Added to True. If the item already exists, then set Added to
   --  False.

   procedure Add (S : in out Unconstrained_Set; I : Item);
   --  Add the item to the set. If the item is not already a distinct
   --  member of the set, copy the item and add it to the set.

   procedure Remove (S : in out Unconstrained_Set; I : Item);
   --  If the item is not a member of the set, raise
   --  BC.Not_Found. Otherwise, remove the item from the set.

   function Extent (S : Unconstrained_Set) return Natural;
   --  Return the number of items in the set.

   function Is_Empty (S : Unconstrained_Set) return Boolean;
   --  Return True if and only if there are no items in the set.

   function Is_Member (S : Unconstrained_Set; I : Item) return Boolean;
   --  Return True if and only if the item exists in the set.

   function New_Iterator
     (For_The_Set : Unconstrained_Set) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Set.

private

   package IC is new BC.Support.Unbounded (Item => Item,
                                           Item_Ptr => Item_Ptr,
                                           Storage => Storage);
   use IC;
   package Items is new BC.Support.Hash_Tables.Item_Signature
     (Item => Item,
      Item_Container => IC.Unb_Node);

   --  We need a dummy type for the Value component of the hash table.
   type Boolean_Ptr is access all Boolean;
   package VC is new BC.Support.Unbounded (Item => Boolean,
                                           Item_Ptr => Boolean_Ptr,
                                           Storage => Storage);
   use VC;
   package Values is new BC.Support.Hash_Tables.Value_Signature
     (Value => Boolean,
      Value_Ptr => Boolean_Ptr,
      Value_Container => VC.Unb_Node);

   package Tables is new BC.Support.Hash_Tables.Tables
     (Items => Items,
      Values => Values);

   type Unconstrained_Set
     (Number_Of_Buckets : Positive)
   is new Abstract_Set with record
      Rep : Tables.Table (Number_Of_Buckets => Number_Of_Buckets);
   end record;

   procedure Attach (S : in out Unconstrained_Set; I : Item);

   procedure Detach (S : in out Unconstrained_Set; I : Item);

   function Number_Of_Buckets (S : Unconstrained_Set) return Natural;

   function Length (S : Unconstrained_Set; Bucket : Positive) return Natural;

   function Item_At (S : Unconstrained_Set;
                     Bucket, Index : Positive) return Item_Ptr;

end BC.Containers.Sets.Unbounded;
