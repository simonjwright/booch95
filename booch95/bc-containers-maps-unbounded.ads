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

with BC.Support.Unbounded;
with BC.Support.Hash_Tables;
with System.Storage_Pools;

generic
  with function Hash (V : Item) return Positive is <>;
  Buckets : Positive;
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Maps.Unbounded is

  -- A map denotes a collection forming a dictionary of domain/range
  -- pairs. Maps are cached, so that the most recently accessed
  -- domain/range pair can be found on the order of O(1).

  -- The hash function (the generic parameter Hash) determines the
  -- allocation of pairs to hash buckets. The value returned must not
  -- change during the lifetime of a given Item. The range of hash values
  -- need not be constrained to the number of buckets in the map.

  -- The hash function must satisfy the condition that, for objects A and
  -- B, if A = B, then Hash (A) must equal Hash (B). The hash function
  -- should attempt to spread the set of possible items uniformly across
  -- the number of buckets. The quality of the hash function has a
  -- significant impact upon performance.

  type Unbounded_Map is new Map with private;

  procedure Clear (M : in out Unbounded_Map);
  -- Empty the map of all item/value pairs. The cached item/value pair is
  -- cleared.

  procedure Bind (M : in out Unbounded_Map; I : Item; V : Value);
  -- If the item already exists in the map, raise BC.Duplicate. Otherwise,
  -- add the item/value pair to the map. The cached item/value pair is set
  -- to this new binding.

  procedure Rebind (M : in out Unbounded_Map; I : Item; V : Value);
  -- If the item does not exist in the map, raise BC.Not_Found. Otherwise,
  -- change the item's binding to the given value. The cached item/value
  -- pair is set to this new binding.

  procedure Unbind (M : in out Unbounded_Map; I : Item);
  -- If the item does not exist in the map, raise BC.Not_Found. Otherwise,
  -- remove the item/value binding. The cached item/value pair is cleared.

  function Extent (M : Unbounded_Map) return Natural;
  -- Return the number of item/value bindings in the map.

  function Is_Empty (M : Unbounded_Map) return Boolean;
  -- Return True if and only if there are no item/value bindings in the
  -- map; otherwise, return False.

  function Is_Bound (M : Unbounded_Map; I : Item) return Boolean;
  -- Return True if and only if there is a binding for the given item in
  -- the map; otherwise, return False. The cached item/value pair is used
  -- to accelerate the search; if there is a cache hit, the time complexity
  -- of this operation is O(1).

  function Value_Of (M : Unbounded_Map; I : Item) return Value;
  -- If the item does not exist in the map, raises BC.Not_Found. Otherwise,
  -- return a constant pointer to the value bound to the given item. The
  -- cached item/value pair is used to accelerate the search; if there is a
  -- cache hit, the time complexity of this operation is O(1).

  function New_Iterator (For_The_Map : Unbounded_Map) return Iterator;
  -- Return a reset Iterator bound to the specific Map.

private

  package IC is new BC.Support.Unbounded (Item => Item,
                                          Item_Ptr => Item_Ptr,
                                          Storage_Manager => Storage_Manager,
                                          Storage => Storage);
  use IC;

  package VC is new BC.Support.Unbounded (Item => Value,
                                          Item_Ptr => Value_Ptr,
                                          Storage_Manager => Storage_Manager,
                                          Storage => Storage);
  use VC;

  package Tables is new BC.Support.Hash_Tables
     (Item => Item,
      Value => Value,
      Value_Ptr => Value_Ptr,
      Buckets => Buckets,
      Item_Container => IC.Unb_Node,
      Item_Container_Ptr => IC.Unb_Node_Ref,
      Value_Container => VC.Unb_Node,
      Value_Container_Ptr => VC.Unb_Node_Ref);

  type Unbounded_Map is new Map with record
    Rep : Tables.Table;
  end record;

  procedure Purge (M : in out Unbounded_Map);

  procedure Attach (M : in out Unbounded_Map; I : Item; V : Value);

  function Cardinality (M : Unbounded_Map) return Natural;

  function Number_Of_Buckets (M : Unbounded_Map) return Natural;

  function Length (M : Unbounded_Map; Bucket : Positive) return Natural;

  function Exists (M : Unbounded_Map; I : Item) return Boolean;

  function Item_At
     (M : Unbounded_Map; Bucket, Index : Positive) return Item_Ptr;

  function Value_At
     (M : Unbounded_Map; Bucket, Index : Positive) return Value_Ptr;

  type Unbounded_Map_Iterator (B : access Unbounded_Map'Class)
  is new Map_Iterator (B) with null record;

end BC.Containers.Maps.Unbounded;

--| //  The C++ Booch Components (Version 2.3)
--| //  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--| //
--| //  BCMapU.h
--| //
--| //  This file contains the declaration of the unbounded map.
--|
--| #ifndef BCMAPU_H
--| #define BCMAPU_H 1
--|
--| #include "BCNodes.h"
--| #include "BCUnboun.h"
--| #include "BCHashTa.h"
--| #include "BCMap.h"
--|
--| // Unbounded map
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| class BC_TUnboundedMap : public BC_TMap<Item, Value> {
--| public:
--|
--|   BC_TUnboundedMap();
--|   BC_TUnboundedMap(BC_Index (*Hash)(const Item&));
--|   BC_TUnboundedMap(const BC_TUnboundedMap<Item, Value, Buckets, StorageManager>&);
--|   virtual ~BC_TUnboundedMap();
--|
--|   virtual BC_TMap<Item, Value>& operator=(const BC_TMap<Item, Value>&);
--|   virtual BC_TMap<Item, Value>&
--|     operator=(const BC_TUnboundedMap<Item, Value, Buckets, StorageManager>&);
--|   virtual BC_Boolean operator==(const BC_TMap<Item, Value>&) const;
--|   virtual BC_Boolean
--|     operator==(const BC_TUnboundedMap<Item, Value, Buckets, StorageManager>&) const;
--|   BC_Boolean
--|     operator!=(const BC_TUnboundedMap<Item, Value, Buckets, StorageManager>&) const;
--|
--|   virtual void SetHashFunction(BC_Index (*Hash)(const Item&));
--|   virtual void Clear();
--|   virtual BC_Boolean Bind(const Item&, const Value&);
--|   virtual BC_Boolean Rebind(const Item&, const Value&);
--|   virtual BC_Boolean Unbind(const Item&);
--|
--|   virtual BC_Index Extent() const;
--|   virtual BC_Boolean IsEmpty() const;
--|   virtual BC_Boolean IsBound(const Item&) const;
--|   virtual const Value* ValueOf(const Item&) const;
--|   virtual Value* ValueOf(const Item&);
--|
--|   static void* operator new(size_t);
--|   static void operator delete(void*, size_t);
--|
--| protected:
--|
--|   BC_TTable<Item, Value, Buckets,
--|             BC_TUnbounded<Item, StorageManager>, BC_TUnbounded<Value,StorageManager> >
--|     fRep;
--|
--|   virtual void Purge();
--|   virtual BC_Boolean Attach(const Item&, const Value&);
--|   virtual BC_Index Cardinality() const;
--|   virtual BC_Index NumberOfBuckets() const;
--|   virtual BC_Index Length(BC_Index bucket) const;
--|   virtual BC_Boolean Exists(const Item&) const;
--|   virtual const Item& ItemAt(BC_Index bucket, BC_Index index) const;
--|   virtual const Value& ValueAt(BC_Index bucket, BC_Index index) const;
--|
--| };
--|
--| #endif
--|
