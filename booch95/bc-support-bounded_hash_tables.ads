--  Copyright (C) 2001-2002 Simon Wright.
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

with Ada.Finalization;

package BC.Support.Bounded_Hash_Tables is

   pragma Elaborate_Body;


   --  In the generic signature packages, Item denotes the universe
   --  from which the hash table draws its items. Value denotes the
   --  universe from which the hash table draws its values. Items and
   --  Values may be either primitive types or user-defined
   --  non-limited types.

   --  The two signature packages use the function Eq for equality
   --  instead of the more normal "=" because ObjectAda 7.2 and 7.2.1
   --  get confused otherwise.

   generic

      type Item is private;
      type Item_Ptr is access all Item;
      with function Eq (L, R : Item) return Boolean;
      with function Hash (V : Item) return Natural is <>;

   package Item_Signature is end Item_Signature;


   generic

      type Value is private;
      type Value_Ptr is access all Value;
      with function Eq (L, R : Value) return Boolean;

   package Value_Signature is end Value_Signature;


   generic

      with package Items is new Item_Signature (<>);
      with package Values is new Value_Signature (<>);

   package Tables is

      --  The type Table represents a closed hash table.

      --  This is a low-level abstraction that specifies no policies
      --  for the order in which items may be added and removed from
      --  the container. This class is not intended to be subclassed.

      subtype Bucket_Index is Positive;
      subtype Index is Natural;
      --  0 => null reference
      subtype Cell_Index is Positive;

      type Cell is record
         Item : Items.Item;
         Value : Values.Value;
         Next : Index;
      end record;

      type Bkts is array (Bucket_Index range <>) of Index;
      type Cells is array (Cell_Index range <>) of Cell;

      type Table (Number_Of_Buckets : Positive; Maximum_Size : Positive)
      is new Ada.Finalization.Controlled with record
         Buckets : Bkts (1 .. Number_Of_Buckets);
         Contents : Cells (1 .. Maximum_Size);
         Size : Natural;
         Free : Index;
      end record;

      procedure Initialize (T : in out Table);

      function "=" (L, R : Table) return Boolean;

      procedure Clear (T : in out Table);
      --  Empty the hash table of all item/value pairs.

      procedure Bind (T : in out Table; I : Items.Item; V : Values.Value);
      --  Generate a hash value for the item to select a bucket. If
      --  the item already exists in that bucket, raise BC.Duplicate;
      --  otherwise, insert the item/value pair in the selected
      --  container.

      procedure Rebind (T : in out Table; I : Items.Item; V : Values.Value);
      --  Generate a hash value for the item to select a bucket. If
      --  the item already exists in that bucket, change the item's
      --  corresponding value; otherwise, raise BC.Not_Found.

      procedure Unbind (T : in out Table; I : Items.Item);
      --  Generate a hash value for the item to select a bucket. If
      --  the item already exists in that bucket, remove the
      --  item/value pair; otherwise, BC.Not_Found.

      function Extent (T : Table) return Natural;
      --  Return the number of item/value pairs in the hash table.

      function Bucket_Extent
        (T : Table; Bucket : Bucket_Index) return Natural;
      --  Return the number of item/value pairs in the selected bucket.

      function Is_Bound (T : Table; I : Items.Item) return Boolean;
      --  Return True if the item has a binding in the hash table;
      --  otherwise, return False.

      function Value_Of (T : Table; I : Items.Item) return Values.Value;
      --  If the item does not have a binding in the hash table, raise
      --  BC.Not_Found; otherwise, return the value corresponding to
      --  this item.

      function Access_Item_At (T : Table; Position : Cell_Index)
                              return Items.Item_Ptr;
      --  Support for iteration.

      function Access_Value_At (T : Table; Position : Cell_Index)
                               return Values.Value_Ptr;
      --  Support for iteration.

   end Tables;

end BC.Support.Bounded_Hash_Tables;
