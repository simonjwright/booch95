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

package BC.Support.Hash_Tables is

   pragma Elaborate_Body;


   --  In the generic signature packages, Item denotes the universe
   --  from which the hash table draws its items. Value denotes the
   --  universe from which the hash table draws its values. Items and
   --  Values may be either primitive types or user-defined
   --  non-limited types.

   --  Item_Container and Value_Container provide the concrete
   --  container for each bucket. These types will normally be
   --  provided by instantiations of the bounded, dynamic, and
   --  unbounded support packages defined for this library.


   generic

      type Item is private;
      type Item_Ptr is access all Item;
      with function "=" (L, R : Item) return Boolean is <>;
      with function Hash (V : Item) return Natural is <>;

      type Item_Container is private;

      --  The <> subprograms for Items are provided by one of
      --  BC.Support.Bounded, Dynamic or Unbounded as appropriate.

      with procedure Clear (C : in out Item_Container) is <>;
      with procedure Insert (C : in out Item_Container; I : Item) is <>;
      with procedure Append (C : in out Item_Container; I : Item) is <>;
      with procedure Remove (C : in out Item_Container; From : Positive) is <>;
      with procedure Replace
        (C : in out Item_Container; Index : Positive; I : Item) is <>;
      with function Length (C : Item_Container) return Natural is <>;
      with function Item_At
        (C : Item_Container; Index : Positive) return Item is <>;
      with function Item_At
        (C : Item_Container; Index : Positive) return Item_Ptr is <>;
      with function Location
        (C : Item_Container; I : Item; Start : Positive) return Natural is <>;

   package Item_Signature is end Item_Signature;


   generic

      type Value is private;
      type Value_Ptr is access all Value;
      with function "=" (L, R : Value) return Boolean is <>;

      type Value_Container is private;

      --  The <> subprograms for Values are provided by one of
      --  BC.Support.Bounded, Dynamic or Unbounded as appropriate.

      with procedure Clear (C : in out Value_Container) is <>;
      with procedure Insert (C : in out Value_Container; V : Value) is <>;
      with procedure Append (C : in out Value_Container; V : Value) is <>;
      with procedure Remove
        (C : in out Value_Container; From : Positive) is <>;
      with procedure Replace
        (C : in out Value_Container; Index : Positive; V : Value) is <>;
      with function Length (C : Value_Container) return Natural is <>;
      with function Item_At
        (C : Value_Container; Index : Positive) return Value is <>;
      with function Item_At
        (C : Value_Container; Index : Positive) return Value_Ptr is <>;
      with function Location
        (C : Value_Container;
         V : Value;
         Start : Positive) return Natural is <>;

   package Value_Signature is end Value_Signature;


   generic

      with package Items is new Item_Signature (<>);
      with package Values is new Value_Signature (<>);

   package Tables is

      --  The type Table represents an open hash table whose buckets
      --  may be formed by bounded, dynamic, or unbounded
      --  containers. Each table contains n buckets, wherein each
      --  bucket is a container of item/value pairs. To insert,
      --  remove, or locate a pair in the table, the operation first
      --  generates a hash value upon the item to select a specific
      --  bucket, and then the given operation is performed upon the
      --  selected container.

      --  This is a low-level abstraction that specifies no policies
      --  for the order in which items may be added and removed from
      --  the container. This class is not intended to be subclassed.

      type Item_Array is array (Positive range <>) of Items.Item_Container;
      type Value_Array is array (Positive range <>) of Values.Value_Container;

      type Table (Number_Of_Buckets : Positive) is record
         Items : Item_Array (1 .. Number_Of_Buckets);
         Values : Value_Array (1 .. Number_Of_Buckets);
         Size : Natural := 0;
      end record;

      function "=" (L, R : Table) return Boolean;

      procedure Clear (T : in out Table);
      --  Empty the hash table of all item/value pairs.

      procedure Bind (T : in out Table; I : Items.Item; V : Values.Value);
      --  Generate a hash value for the item to select a bucket. If
      --  the item already exists in that bucket, raise BC.Duplicate;
      --  otherwise, insert the Item/value pair in the selected
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

      function Is_Bound (T : Table; I : Items.Item) return Boolean;
      --  Return True if the item has a binding in the hash table;
      --  otherwise, return False.

      function Value_Of (T : Table; I : Items.Item) return Values.Value;
      --  If the item does not have a binding in the hash table, raise
      --  BC.Not_Found; otherwise, return the value corresponding to
      --  this item.

      --  Iterator support

      procedure Reset (T : Table;
                       Bucket : out Positive;
                       Index : out Positive);

      function Is_Done (T : Table;
                        Bucket : Positive;
                        Index : Positive) return Boolean;

      function Current_Item_Ptr (T : Table;
                                 Bucket : Positive;
                                 Index : Positive) return Items.Item_Ptr;

      function Current_Value_Ptr (T : Table;
                                  Bucket : Positive;
                                  Index : Positive) return Values.Value_Ptr;

      procedure Delete_Item_At (T : in out Table;
                                Bucket : in out Positive;
                                Index : in out  Positive);

      procedure Next (T : Table;
                      Bucket : in out Positive;
                      Index : in out  Positive);

   end Tables;

end BC.Support.Hash_Tables;
