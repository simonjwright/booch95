-- Copyright (C) 1994-1998 Grady Booch and Simon Wright.
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

with Ada.Finalization;

generic
  type Item is private;
  type Item_Ptr is access all Item;
  with function "=" (L, R : Item) return Boolean is <>;
  with function Hash (V : Item) return Positive is <>;
  type Value is private;
  type Value_Ptr is access all Value;
  with function "=" (L, R : Value) return Boolean is <>;
  Buckets : Natural;
  type Item_Container is private;
  type Item_Container_Ptr is access all Item_Container;
--  with function Create (From : Item_Container) return Item_Container is <>;
  with procedure Clear (C : in out Item_Container) is <>;
  with procedure Insert (C : in out Item_Container; I : Item) is <>;
  with procedure Append (C : in out Item_Container; I : Item) is <>;
  with procedure Remove (C : in out Item_Container; From : Positive) is <>;
  with procedure Replace
     (C : in out Item_Container; Index : Positive; I : Item) is <>;
  with function Length (C : Item_Container) return Natural is <>;
  with function Item_At
     (C : Item_Container; Index : Positive) return Item is <>;
  with function Location
     (C : Item_Container; I : Item; Start : Positive) return Natural is <>;
  type Value_Container is private;
  type Value_Container_Ptr is access all Value_Container;
--  with function Create (From : Value_Container) return Value_Container is <>;
  with procedure Clear (C : in out Value_Container) is <>;
  with procedure Insert (C : in out Value_Container; V : Value) is <>;
  with procedure Append (C : in out Value_Container; V : Value) is <>;
  with procedure Remove (C : in out Value_Container; From : Positive) is <>;
  with procedure Replace
     (C : in out Value_Container; Index : Positive; V : Value) is <>;
  with function Length (C : Value_Container) return Natural is <>;
  with function Item_At
     (C : Value_Container; Index : Positive) return Value is <>;
  with function Item_At
     (C : Value_Container; Index : Positive) return Value_Ptr is <>;
  with function Location
     (C : Value_Container; V : Value; Start : Positive) return Natural is <>;
package BC.Support.Hash_Tables is

  -- The required operations appear to be Length(), Append(), ItemAt(), Clear(), Location(), Insert(), Replace(), Remove(), []

--| //  The C++ Booch Components (Version 2.3)
--| //  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--| //
--| //  BCHashTa.h
--| //
--| //  This file contains the declaration of the open hash table class.
--|  
--| #ifndef BCHASHTA_H
--| #define BCHASHTA_H 1
--| 
--| #include "BCType.h"
--| #include "BCExcept.h"
--| #include "BCNodes.h"
--| 
--| // Class denoting an open hash table
--| 
--| template<class Item, class Value, BC_Index Buckets, 
--|          class ItemContainer, class ValueContainer>
--| class BC_TTable {
  --| public:

  -- The type Table represents an open hash table whose buckets may be
  -- formed by bounded, dynamic, or unbounded containers. Each table
  -- contains n buckets, wherein each bucket is a container of item/value
  -- pairs. To insert, remove, or locate a pair in the table, the operation
  -- first generates a hash value upon the item to select a specific
  -- bucket, and then the given operation is performed upon the selected
  -- container.

  -- This is a low-level abstraction that specifies no policies for the
  -- order in which items may be added and removed from the container. This
  -- class is not intended to be subclassed.

  -- In the generic parameters, Item denotes the universe from which the
  -- hash table draws its items. Value denotes the
  -- universe from which the hash table draws its values. Items and values
  -- may be either primitive types or user-defined non-limited types.

  -- The parameter Buckets signifies the static number of buckets in the
  -- hash table.

  -- The parameters Item_Container and Value_Container provide the concrete
  -- container for each bucket. These classes will normally be provided by
  -- instantiations of the bounded, dynamic, and unbounded support packages
  -- defined for this library.

  type Table is new Ada.Finalization.Controlled with private;

--| 
--|   BC_TTable()
--|     : fSize(0),
--|       fHash(0) {}
--|   BC_TTable(BC_Index (*hash)(const Item&))
--|     : fSize(0),
--|       fHash(hash) {}
--|   BC_TTable(const BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>&);
--|   ~BC_TTable()
--|     {Clear();}
--X this is the Finalize()
--| 
--|   BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>& operator=
--|     (const BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>&);
--|   BC_Boolean operator==
--|     (const BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>&) const;

  function "=" (L, R : Table) return Boolean;

--|   BC_Boolean operator!=
--|     (const BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>& t) const
--|       {return !operator==(t);}
--| 
--|   void SetHashFunction(BC_Index (*hash)(const Item&))
--|     {BC_Assert((!fHash), BC_XDuplicate("BC_TTable::SetHashFunction", BC_kDuplicate));
--|      fHash = hash;}
--|   void Clear();

  procedure Clear (T : in out Table);
  -- Empty the hash table of all item/value pairs. 

--|   BC_Boolean Bind(const Item&, const Value&);

  procedure Bind (T : in out Table; I : Item; V : Value);
  -- Generate a hash value for the item to select a bucket. If the item
  -- already exists in that bucket, raise BC.Duplicate; otherwise, insert
  -- the Item/value pair in the selected container.

--|   BC_Boolean Rebind(const Item&, const Value&);

  procedure Rebind (T : in out Table; I : Item; V : Value);
  -- Generate a hash value for the item to select a bucket. If the item
  -- already exists in that bucket, change the item's corresponding value;
  -- otherwise, raise BC.Not_Found.

--|   BC_Boolean Unbind(const Item&);

  procedure Unbind (T : in out Table; I : Item);
  -- Generate a hash value for the item to select a bucket. If the item
  -- already exists in that bucket, remove the item/value pair; otherwise,
  -- BC.Not_Found.

--| 
--|   BC_Index Extent() const
--|     {return fSize;}

  function Extent (T : Table) return Natural;
  -- Return the number of item/value pairs in the hash table. 

--|   BC_Boolean IsBound(const Item&) const;

  function Is_Bound (T : Table; I : Item) return Boolean;
  -- Return True if the item has a binding in the hash table; otherwise,
  -- return False.

--|   const Value* ValueOf(const Item&) const;

  function Value_Of (T : Table; I : Item) return Value;
  -- If the item does not have a binding in the hash table, raise
  -- BC.Not_Found; otherwise, return the value corresponding to this item.

  function Value_Of (T : Table; I : Item) return Value_Ptr;
  -- If the item does not have a binding in the hash table, raise
  -- BC.Not_Found; otherwise, return a pointer to the value corresponding
  -- to this item.

--|   const ItemContainer *const ItemBucket(BC_Index bucket) const
--|     {BC_Assert((bucket < Buckets),
--|                BC_XRangeError("BC_TTable::ItemBucket", BC_kInvalidIndex));
--|      return &fItemRep[bucket];}

  function Item_Bucket
     (T : Table; Bucket : Positive) return Item_Container_Ptr;

--|   ItemContainer* ItemBucket(BC_Index bucket)
--|     {BC_Assert((bucket < Buckets),
--|                BC_XRangeError("BC_TTable::ItemBucket", BC_kInvalidIndex));
--|      return &fItemRep[bucket];}

--|   const ValueContainer *const ValueBucket(BC_Index bucket) const
--|     {BC_Assert((bucket < Buckets),
--|                BC_XRangeError("BC_TTable::ValueBucket", BC_kInvalidIndex));
--|      return &fValueRep[bucket];}

  function Value_Bucket
     (T : Table; Bucket : Positive) return Value_Container_Ptr;

--|   ValueContainer* ValueBucket(BC_Index bucket)
--|     {BC_Assert((bucket < Buckets),
--|                BC_XRangeError("BC_TTable::ItemBucket", BC_kInvalidIndex));
--|      return &fValueRep[bucket];}

--| 
--| protected:
--| 
--|   ItemContainer fItemRep[Buckets];
--|   ValueContainer fValueRep[Buckets];
--|   BC_Index fSize;
--|   BC_Index (*fHash)(const Item&);
--|   
--| };

private

  type Item_Array is array (1 .. Buckets) of aliased Item_Container;
  type Value_Array is array (1 .. Buckets) of aliased Value_Container;

  type Table is new Ada.Finalization.Controlled with record
    Items : Item_Array;
    Values : Value_Array;
    Size : Natural := 0;
  end record;

  procedure Finalize (T : in out Table);

--| 
--| #endif

end BC.Support.Hash_Tables;
