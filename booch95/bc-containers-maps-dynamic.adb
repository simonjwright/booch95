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

with System.Address_To_Access_Conversions;

package body BC.Containers.Maps.Dynamic is

  function Create (Size : Positive) return Dynamic_Map is
    M : Dynamic_Map;
  begin
    for B in 1 .. Buckets loop
      IC.Set_Chunk_Size (Tables.Item_Bucket (M.Rep, B).all, Size);
      VC.Set_Chunk_Size (Tables.Value_Bucket (M.Rep, B).all, Size);
    end loop;
    return M;
  end Create;

  procedure Clear (M : in out Dynamic_Map) is
  begin
    Tables.Clear (M.Rep);
  end Clear;

  procedure Bind
     (M : in out Dynamic_Map; I : Item; V : Value) is
  begin
    Tables.Bind (M.Rep, I, V);
  end Bind;

  procedure Rebind
     (M : in out Dynamic_Map; I : Item; V : Value) is
  begin
    Tables.Rebind (M.Rep, I, V);
  end Rebind;

  procedure Unbind (M : in out Dynamic_Map; I : Item) is
  begin
    Tables.Unbind (M.Rep, I);
  end Unbind;

  function Extent (M : Dynamic_Map) return Natural is
  begin
    return Tables.Extent (M.Rep);
  end Extent;

  function Is_Empty (M : Dynamic_Map) return Boolean is
  begin
    return Tables.Extent (M.Rep) = 0;
  end Is_Empty;

  function Is_Bound (M : Dynamic_Map; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (M.Rep, I);
  end Is_Bound;

  function Value_Of (M : Dynamic_Map; I : Item) return Value is
  begin
    return Tables.Value_Of (M.Rep, I);
  end Value_Of;

  procedure Preallocate (M : in out Dynamic_Map; Size : Positive) is
  begin
    for B in 1 .. Buckets loop
      IC.Preallocate (Tables.Item_Bucket (M.Rep, B).all, Size);
      VC.Preallocate (Tables.Value_Bucket (M.Rep, B).all, Size);
    end loop;
  end Preallocate;

  procedure Set_Chunk_Size (M : in out Dynamic_Map; Size : Positive) is
  begin
    for B in 1 .. Buckets loop
      IC.Set_Chunk_Size (Tables.Item_Bucket (M.Rep, B).all, Size);
      VC.Set_Chunk_Size (Tables.Value_Bucket (M.Rep, B).all, Size);
    end loop;
  end Set_Chunk_Size;

  function Chunk_Size (M : Dynamic_Map) return Positive is
  begin
    return IC.Chunk_Size (Tables.Item_Bucket (M.Rep, 1).all);
  end Chunk_Size;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Dynamic_Map);

  function New_Iterator (For_The_Map : Dynamic_Map) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Map'Address);
  begin
    return Iterator (SP.Create (new Dynamic_Map_Iterator (P)));
  end New_Iterator;

  -- Private implementations

  procedure Purge (M : in out Dynamic_Map) is
  begin
    Tables.Clear (M.Rep);
  end Purge;

  procedure Attach (M : in out Dynamic_Map; I : Item; V : Value) is
  begin
    Tables.Bind (M.Rep, I, V);
  end Attach;

  function Cardinality (M : Dynamic_Map) return Natural is
  begin
    return Tables.Extent (M.Rep);
  end Cardinality;

  function Number_Of_Buckets (M : Dynamic_Map) return Natural is
  begin
    return Buckets;
  end Number_Of_Buckets;

  function Length (M : Dynamic_Map; Bucket : Positive) return Natural is
  begin
    return IC.Length (Tables.Item_Bucket (M.Rep, Bucket).all);
  end Length;

  function Exists (M : Dynamic_Map; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (M.Rep, I);
  end Exists;

  function Item_At
     (M : Dynamic_Map; Bucket, Index : Positive) return Item_Ptr is
  begin
    return IC.Item_At (Tables.Item_Bucket (M.Rep, Bucket).all, Index);
  end Item_At;

  function Value_At
     (M : Dynamic_Map; Bucket, Index : Positive) return Value_Ptr is
  begin
    return VC.Item_At (Tables.Value_Bucket (M.Rep, Bucket).all, Index);
  end Value_At;

  -- This looks like some sort of copy constructor; I can't quite make
  -- it out ..

--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   BC_TDynamicMap(const BC_TDynamicMap<Item, Value, Buckets, StorageManager>& m)
--| {
--|   for (BC_Index index = 0; (index < Buckets); index++) {
--|     fRep.ItemBucket(index)->SetChunkSize(m.fRep.ItemBucket(index)->ChunkSize());
--|     fRep.ValueBucket(index)->SetChunkSize(m.fRep.ValueBucket(index)->ChunkSize());
--|   }
--|   fRep = m.fRep;
--| }

  -- .. given the implementation of equality ..

--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   operator==(const BC_TDynamicMap<Item, Value, Buckets, StorageManager>& m) const
--| {
--|   return (fRep == m.fRep);
--| }

end BC.Containers.Maps.Dynamic;

--| //  The C++ Booch Components (Version 2.3)
--| //  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--| //
--| //  Restricted Rights Legend
--| //  Use, duplication, or disclosure is subject to restrictions as bag forth
--| //  in subdivision (c)(1)(ii) of the Rights in Technical Data and Computer
--| //  Software clause at DFARS 252.227-7013.
--| //
--| //  BCMapD.cpp
--| //
--| //  This file contains the definitions for the dynamic map.
--|
--| #include "BCMapD.h"
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_TDynamicMap<Item, Value, Buckets, StorageManager>::BC_TDynamicMap() {}
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   BC_TDynamicMap(BC_Index (*Hash)(const Item&))
--|     : fRep(Hash) {}
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   BC_TDynamicMap(BC_Index chunkSize)
--| {
--|   for (BC_Index index = 0; (index < Buckets); index++) {
--|     fRep.ItemBucket(index)->SetChunkSize(chunkSize);
--|     fRep.ValueBucket(index)->SetChunkSize(chunkSize);
--|   }
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   BC_TDynamicMap(const BC_TDynamicMap<Item, Value, Buckets, StorageManager>& m)
--| {
--|   for (BC_Index index = 0; (index < Buckets); index++) {
--|     fRep.ItemBucket(index)->SetChunkSize(m.fRep.ItemBucket(index)->ChunkSize());
--|     fRep.ValueBucket(index)->SetChunkSize(m.fRep.ValueBucket(index)->ChunkSize());
--|   }
--|   fRep = m.fRep;
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_TDynamicMap<Item, Value, Buckets, StorageManager>::~BC_TDynamicMap() {}
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_TMap<Item, Value>& BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   operator=(const BC_TMap<Item, Value>& m)
--| {
--|   return BC_TMap<Item, Value>::operator=(m);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_TMap<Item, Value>& BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   operator=(const BC_TDynamicMap<Item, Value, Buckets, StorageManager>& m)
--| {
--|   fRep = m.fRep;
--|   return *this;
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   operator==(const BC_TMap<Item, Value>& m) const
--| {
--|   return BC_TMap<Item, Value>::operator==(m);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   operator==(const BC_TDynamicMap<Item, Value, Buckets, StorageManager>& m) const
--| {
--|   return (fRep == m.fRep);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   operator!=(const BC_TDynamicMap<Item, Value, Buckets, StorageManager>& m) const
--| {
--|   return !operator==(m);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| void BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   SetHashFunction(BC_Index (*Hash)(const Item&))
--| {
--|   fRep.SetHashFunction(Hash);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| void BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   SetChunkSize(BC_Index chunkSize)
--| {
--|   for (BC_Index index = 0; (index < Buckets); index++) {
--|     fRep.ItemBucket(index)->SetChunkSize(chunkSize);
--|     fRep.ValueBucket(index)->SetChunkSize(chunkSize);
--|   }
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| void BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   Preallocate(BC_Index new_length)
--| {
--|   for (BC_Index index = 0; (index < Buckets); index++) {
--|     fRep.ItemBucket(index)->Preallocate(new_length);
--|     fRep.ValueBucket(index)->Preallocate(new_length);
--|   }
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| void BC_TDynamicMap<Item, Value, Buckets, StorageManager>::Clear()
--| {
--|   fRep.Clear();
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   Bind(const Item& item, const Value& value)
--| {
--|   return fRep.Bind(item, value);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   Rebind(const Item& item, const Value& value)
--| {
--|   return fRep.Rebind(item, value);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   Unbind(const Item& item)
--| {
--|   return fRep.Unbind(item);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Index BC_TDynamicMap<Item, Value, Buckets, StorageManager>::ChunkSize() const
--| {
--|   return fRep.ItemBucket(0)->ChunkSize();
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Index BC_TDynamicMap<Item, Value, Buckets, StorageManager>::Extent() const
--| {
--|   return fRep.Extent();
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::IsEmpty() const
--| {
--|   return (fRep.Extent() == 0);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   IsBound(const Item& item) const
--| {
--|   return fRep.IsBound(item);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| const Value* BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   ValueOf(const Item& item) const
--| {
--|   return fRep.ValueOf(item);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| Value* BC_TDynamicMap<Item, Value, Buckets, StorageManager>::ValueOf(const Item& item)
--| {
--|   return (Value*)(fRep.ValueOf(item));
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| void* BC_TDynamicMap<Item, Value, Buckets, StorageManager>::operator new(size_t s)
--| {
--|   return StorageManager::Allocate(s);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| void BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   operator delete(void* p, size_t s)
--| {
--|   StorageManager::Deallocate(p, s);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| void BC_TDynamicMap<Item, Value, Buckets, StorageManager>::Purge()
--| {
--|   fRep.Clear();
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   Attach(const Item& item, const Value& value)
--| {
--|   return fRep.Bind(item, value);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Index BC_TDynamicMap<Item, Value, Buckets, StorageManager>::Cardinality() const
--| {
--|   return fRep.Extent();
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Index BC_TDynamicMap<Item, Value, Buckets, StorageManager>::NumberOfBuckets() const
--| {
--|   return Buckets;
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Index BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   Length(BC_Index bucket) const
--| {
--|   return fRep.ItemBucket(bucket)->Length();
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| BC_Boolean BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   Exists(const Item& item) const
--| {
--|   return fRep.IsBound(item);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| const Item& BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   ItemAt(BC_Index bucket, BC_Index index) const
--| {
--|   return (*fRep.ItemBucket(bucket))[index];
--| }
--|
--| template<class Item, class Value, BC_Index Buckets, class StorageManager>
--| const Value& BC_TDynamicMap<Item, Value, Buckets, StorageManager>::
--|   ValueAt(BC_Index bucket, BC_Index index) const
--| {
--|   return (*fRep.ValueBucket(bucket))[index];
--| }
--|
