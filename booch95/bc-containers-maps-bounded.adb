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

package body BC.Containers.Maps.Bounded is

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| void BC_TBoundedMap<Item, Value, Buckets, Size>::Clear()
--| {
--|   fRep.Clear();
--| }

  procedure Clear (M : in out Bounded_Map) is
  begin
    Tables.Clear (M.Rep);
  end Clear;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Boolean BC_TBoundedMap<Item, Value, Buckets, Size>::
--|   Bind(const Item& item, const Value& value)
--| {
--|   return fRep.Bind(item, value);
--| }

  procedure Bind
     (M : in out Bounded_Map; I : Item; V : Value) is
  begin
    Tables.Bind (M.Rep, I, V);
  end Bind;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Boolean BC_TBoundedMap<Item, Value, Buckets, Size>::
--|   Rebind(const Item& item, const Value& value)
--| {
--|   return fRep.Rebind(item, value);
--| }

  procedure Rebind
     (M : in out Bounded_Map; I : Item; V : Value) is
  begin
    Tables.Rebind (M.Rep, I, V);
  end Rebind;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Boolean BC_TBoundedMap<Item, Value, Buckets, Size>::Unbind(const Item& item)
--| {
--|   return fRep.Unbind(item);
--| }

  procedure Unbind (M : in out Bounded_Map; I : Item) is
  begin
    Tables.Unbind (M.Rep, I);
  end Unbind;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Index BC_TBoundedMap<Item, Value, Buckets, Size>::Available() const
--| {
--|   BC_Index count = 0;
--|   for (BC_Index index = 0; (index < Buckets); index++)
--|     count += fRep.ItemBucket(index)->Available();
--|   return count;
--| }

  function Available (M : Bounded_Map) return Natural is
    Count : Natural := 0;
  begin
    for B in 1 .. Buckets loop
      Count := Count + IC.Available (Tables.Item_Bucket (M.Rep, B).all);
    end loop;
    return Count;
  end Available;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Index BC_TBoundedMap<Item, Value, Buckets, Size>::Extent() const
--| {
--|   return fRep.Extent();
--| }

  function Extent (M : Bounded_Map) return Natural is
  begin
    return Tables.Extent (M.Rep);
  end Extent;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Boolean BC_TBoundedMap<Item, Value, Buckets, Size>::IsEmpty() const
--| {
--|   return (fRep.Extent() == 0);
--| }

  function Is_Empty (M : Bounded_Map) return Boolean is
  begin
    return Tables.Extent (M.Rep) = 0;
  end Is_Empty;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Boolean BC_TBoundedMap<Item, Value, Buckets, Size>::IsBound(const Item& item) const
--| {
--|   return fRep.IsBound(item);
--| }

  function Is_Bound (M : Bounded_Map; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (M.Rep, I);
  end Is_Bound;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| const Value* BC_TBoundedMap<Item, Value, Buckets, Size>::ValueOf(const Item& item) const
--| {
--|   return fRep.ValueOf(item);
--| }

  function Value_Of (M : Bounded_Map; I : Item) return Value is
  begin
    return Tables.Value_Of (M.Rep, I);
  end Value_Of;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Bounded_Map);

  function New_Iterator (For_The_Map : Bounded_Map) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Map'Address);
  begin
    return Iterator (SP.Create (new Bounded_Map_Iterator (P)));
  end New_Iterator;

  -- Private implementations

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| void BC_TBoundedMap<Item, Value, Buckets, Size>::Purge()
--| {
--|   fRep.Clear();
--| }

  procedure Purge (M : in out Bounded_Map) is
  begin
    Tables.Clear (M.Rep);
  end Purge;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Boolean BC_TBoundedMap<Item, Value, Buckets, Size>::
--|   Attach(const Item& item, const Value& value)
--| {
--|   return fRep.Bind(item, value);
--| }

  procedure Attach (M : in out Bounded_Map; I : Item; V : Value) is
  begin
    Tables.Bind (M.Rep, I, V);
  end Attach;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Index BC_TBoundedMap<Item, Value, Buckets, Size>::Cardinality() const
--| {
--|   return fRep.Extent();
--| }

  function Cardinality (M : Bounded_Map) return Natural is
  begin
    return Tables.Extent (M.Rep);
  end Cardinality;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Index BC_TBoundedMap<Item, Value, Buckets, Size>::NumberOfBuckets() const
--| {
--|   return Buckets;
--| }

  function Number_Of_Buckets (M : Bounded_Map) return Natural is
  begin
    return Buckets;
  end Number_Of_Buckets;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Index BC_TBoundedMap<Item, Value, Buckets, Size>::Length(BC_Index bucket) const
--| {
--|   return fRep.ItemBucket(bucket)->Length();
--| }

  function Length (M : Bounded_Map; Bucket : Positive) return Natural is
  begin
    return IC.Length (Tables.Item_Bucket (M.Rep, Bucket).all);
  end Length;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| BC_Boolean BC_TBoundedMap<Item, Value, Buckets, Size>::Exists(const Item& item) const
--| {
--|   return fRep.IsBound(item);
--| }

  function Exists (M : Bounded_Map; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (M.Rep, I);
  end Exists;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| const Item& BC_TBoundedMap<Item, Value, Buckets, Size>::
--|   ItemAt(BC_Index bucket, BC_Index index) const
--| {
--|   return (*fRep.ItemBucket(bucket))[index];
--| }

  function Item_At
     (M : Bounded_Map; Bucket, Index : Positive) return Item_Ptr is
  begin
    return IC.Item_At (Tables.Item_Bucket (M.Rep, Bucket).all, Index);
  end Item_At;

--| template<class Item, class Value, BC_Index Buckets, BC_Index Size>
--| const Value& BC_TBoundedMap<Item, Value, Buckets, Size>::
--|   ValueAt(BC_Index bucket, BC_Index index) const
--| {
--|   return (*fRep.ValueBucket(bucket))[index];
--| }

  function Value_At
     (M : Bounded_Map; Bucket, Index : Positive) return Value_Ptr is
  begin
    return VC.Item_At (Tables.Value_Bucket (M.Rep, Bucket).all, Index);
  end Value_At;

end BC.Containers.Maps.Bounded;
