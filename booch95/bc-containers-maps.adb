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

with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Containers.Maps is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Maps");

--| //  The C++ Booch Components (Version 2.3)
--| //  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--| //
--| //  Restricted Rights Legend
--| //  Use, duplication, or disclosure is subject to restrictions as bag forth
--| //  in subdivision (c)(1)(ii) of the Rights in Technical Data and Computer
--| //  Software clause at DFARS 252.227-7013.
--| //
--| //  BCMap.cpp
--| //
--| //  This file contains the definitions for the map abstract base class
--| //  and its iterators.
--|
--| #include "BCMap.h"
--|
--| template<class Item, class Value>
--| BC_TMap<Item, Value>::BC_TMap() {}
--|
--| template<class Item, class Value>
--| BC_TMap<Item, Value>::BC_TMap(BC_Index (*)(const Item&)) {}
--|
--| template<class Item, class Value>
--| BC_TMap<Item, Value>::BC_TMap(const BC_TMap<Item, Value>&) {}
--|
--| template<class Item, class Value>
--| BC_TMap<Item, Value>::~BC_TMap() {}
--|
--| template<class Item, class Value>
--| BC_TMap<Item, Value>& BC_TMap<Item, Value>::operator=(const BC_TMap<Item, Value>& m)
--| {
--|   if (this == &m)
--|     return *this;
--|   ((BC_TMap<Item, Value>&)m).Lock();
--|   Purge();
--|   BC_TMapActiveIterator<Item, Value> iter(m);
--|   while (!iter.IsDone()) {
--|     Attach(*iter.CurrentItem(), *iter.CurrentValue());
--|     iter.Next();
--|   }
--|   ((BC_TMap<Item, Value>&)m).Unlock();
--|   return *this;
--| }
--|
--| template<class Item, class Value>
--| BC_Boolean BC_TMap<Item, Value>::operator==(const BC_TMap<Item, Value>& m) const
--| {
--|   if (this == &m)
--|     return 1;
--|   ((BC_TMap<Item, Value>&)m).Lock();
--|   if (Cardinality() != m.Cardinality()) {
--|     ((BC_TMap<Item, Value>&)m).Unlock();
--|     return 0;
--|   }
--|   BC_TMapActiveIterator<Item, Value> iter(*this);
--|   while (!iter.IsDone()) {
--|     if (!m.Exists(*iter.CurrentItem())) {
--|       ((BC_TMap<Item, Value>&)m).Unlock();
--|       return 0;
--|     }
--|     iter.Next();
--|   }
--|   ((BC_TMap<Item, Value>&)m).Unlock();
--|   return 1;
--| }

--    function "=" (L, R : Map'Class) return Boolean is
--    begin
--      if Cardinality (L) /= Cardinality (R) then
--        return False;
--      end if;
--      declare
--        -- We can't just declare our iterator by reference to L'Access,
--        -- because L is an in parameter and hence constant.
--        package Conversions is new System.Address_To_Access_Conversions
--           (Map'Class);
--        P : Conversions.Object_Pointer := Conversions.To_Pointer (L'Address);
--        It : Map_Iterator (P.all'Access);
--      begin
--        while not Is_Done (It) loop
--          if not Exists (R, Current_Item (It)) then
--            return False;
--          end if;
--          -- XXX what about the Value?
--          Next (It);
--        end loop;
--        return True;
--      end;
--    end "=";

  function Are_Equal (L, R : Map'Class) return Boolean is
    It : Iterator := New_Iterator (L);
  begin
    if Cardinality (L) /= Cardinality (R) then
      return False;
    end if;
    while not Is_Done (It) loop
      if not Exists (R, Current_Item (It)) then
	return False;
      end if;
      -- XXX what about the Value?
      Next (It);
    end loop;
    return True;
  end Are_Equal;

--| template<class Item, class Value>
--| BC_Boolean BC_TMap<Item, Value>::operator!=(const BC_TMap<Item, Value>& m) const
--| {
--|   return !operator==(m);
--| }
--|
--| template<class Item, class Value>
--| void BC_TMap<Item, Value>::Lock() {}
--|
--| template<class Item, class Value>
--| void BC_TMap<Item, Value>::Unlock() {}

--| template<class Item, class Value>
--| BC_TMapActiveIterator<Item, Value>::
--|   BC_TMapActiveIterator(const BC_TMap<Item, Value>& m)
--|     : fMap(m),
--|       fBucketIndex(m.Cardinality() ? 0 : -1),
--|       fIndex(-1)
--| {
--|   if (fBucketIndex == 0) {
--|     for (; (fBucketIndex < fMap.NumberOfBuckets()); fBucketIndex++) {
--|       fIndex = fMap.Length(fBucketIndex) ? 0 : -1;
--|       if (fIndex != -1)
--|         return;
--|     }
--|   }
--| }

  procedure Initialize (It : in out Map_Iterator) is
  begin
    It.Index := 0;
    if Cardinality (It.M.all) = 0 then
      It.Bucket_Index := 0;
    else
      It.Bucket_Index := 1;
      while It.Bucket_Index <= Number_Of_Buckets (It.M.all) loop
        if Length (It.M.all, It.Bucket_Index) > 0 then
          It.Index := 1;
          exit;
        end if;
        It.Bucket_Index := It.Bucket_Index + 1;
      end loop;
    end if;
  end Initialize;

--| template<class Item, class Value>
--| BC_TMapActiveIterator<Item, Value>::~BC_TMapActiveIterator() {}
--|
--| template<class Item, class Value>
--| void BC_TMapActiveIterator<Item, Value>::Reset()
--| {
--|   fBucketIndex = fMap.Cardinality() ? 0 : -1;
--|   fIndex = -1;
--|   if (fBucketIndex == 0) {
--|     for (; (fBucketIndex < fMap.NumberOfBuckets()); fBucketIndex++) {
--|       fIndex = fMap.Length(fBucketIndex) ? 0 : -1;
--|       if (fIndex != -1)
--|         return;
--|     }
--|   }
--| }

  procedure Reset (It : in out Map_Iterator) is
  begin
    It.Index := 0;
    if Cardinality (It.M.all) = 0 then
      It.Bucket_Index := 0;
    else
      It.Bucket_Index := 1;
      while It.Bucket_Index <= Number_Of_Buckets (It.M.all) loop
        if Length (It.M.all, It.Bucket_Index) > 0 then
          It.Index := 1;
          exit;
        end if;
        It.Bucket_Index := It.Bucket_Index + 1;
      end loop;
    end if;
  end Reset;

--| template<class Item, class Value>
--| BC_Boolean BC_TMapActiveIterator<Item, Value>::Next()
--| {
--|   if (fBucketIndex < fMap.NumberOfBuckets()) {
--|     fIndex++;
--|     if (fIndex >= fMap.Length(fBucketIndex)) {
--|       fBucketIndex++;
--|       fIndex = - 1;
--|       for (; (fBucketIndex < fMap.NumberOfBuckets()); fBucketIndex++) {
--|         fIndex = fMap.Length(fBucketIndex) ? 0 : -1;
--|         if (fIndex != -1)
--|           break;
--|       }
--|     }
--|     return !IsDone();
--|   } else
--|     return 0;
--| }

  procedure Next (It : in out Map_Iterator) is
  begin
    if It.Bucket_Index <= Number_Of_Buckets (It.M.all) then
      if It.Index < Length (It.M.all, It.Bucket_Index) then
        It.Index := It.Index + 1;
      else
        It.Bucket_Index := It.Bucket_Index + 1;
        It.Index := 0;
        while It.Bucket_Index <= Number_Of_Buckets (It.M.all) loop
          if Length (It.M.all, It.Bucket_Index) > 0 then
            It.Index := 1;
            exit;
          end if;
          It.Bucket_Index := It.Bucket_Index + 1;
        end loop;
      end if;
    end if;
  end Next;

--| template<class Item, class Value>
--| BC_Boolean BC_TMapActiveIterator<Item, Value>::IsDone() const
--| {
--|   if ((fBucketIndex < 0) || (fBucketIndex >= fMap.NumberOfBuckets()))
--|     return 1;
--|   else {
--|     if (fIndex >= fMap.Length(fBucketIndex)) {
--|       ((BC_TMapActiveIterator<Item, Value>&)(*this)).fBucketIndex++;
--|       ((BC_TMapActiveIterator<Item, Value>&)(*this)).fIndex = -1;
--|       for (; (fBucketIndex < fMap.NumberOfBuckets());
--|            ((BC_TMapActiveIterator<Item, Value>&)(*this)).fBucketIndex++) {
--|         ((BC_TMapActiveIterator<Item, Value>&)(*this)).fIndex =
--|           fMap.Length(fBucketIndex) ? 0 : - 1;
--|         if (fIndex != -1)
--|           return 0;
--|       }
--|       return 1;
--|     }
--|     return 0;
--|   }
--| }

  function Is_Done (It : Map_Iterator) return Boolean is
  begin
    if It.Bucket_Index = 0
       or else It.Bucket_Index > Number_Of_Buckets (It.M.all) then
      return True;
    end if;
    if It.Index <= Length (It.M.all, It.Bucket_Index) then
      return False;
    end if;
    declare
      package Conversions is new System.Address_To_Access_Conversions
         (Map_Iterator'Class);
      P : Conversions.Object_Pointer := Conversions.To_Pointer (It'Address);
    begin
      P.Bucket_Index := P.Bucket_Index + 1;
      P.Index := 0;
      while P.Bucket_Index <= Number_Of_Buckets (P.M.all) loop
        if Length (P.M.all, P.Bucket_Index) > 0 then
          P.Index := 1;
          return False;
        end if;
        P.Bucket_Index := P.Bucket_Index + 1;
      end loop;
    end;
    return True;
  end Is_Done;

--| template<class Item, class Value>
--| const Item* BC_TMapActiveIterator<Item, Value>::CurrentItem() const
--| {
--|   return IsDone() ? 0 : &fMap.ItemAt(fBucketIndex, fIndex);
--| }

  function Current_Item (It : Map_Iterator) return Item is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.M.all, It.Bucket_Index, It.Index).all;
  end Current_Item;

--| template<class Item, class Value>
--| Item* BC_TMapActiveIterator<Item, Value>::CurrentItem()
--| {
--|   return IsDone() ? 0 : &((Item&)(fMap.ItemAt(fBucketIndex, fIndex)));
--| }

  function Current_Value (It : Iterator) return Value is
    Map_Iter : Map_Iterator
       renames Map_Iterator (SP.Value (SP.Pointer (It)).all);
  begin
    if Is_Done (Map_Iter) then
      raise BC.Not_Found;
    end if;
    return Value_At (Map_Iter.M.all, Map_Iter.Bucket_Index, Map_Iter.Index).all;
  end Current_Value;

--| template<class Item, class Value>
--| const Value* BC_TMapActiveIterator<Item, Value>::CurrentValue() const
--| {
--|   return IsDone() ? 0 : &fMap.ValueAt(fBucketIndex, fIndex);
--| }
--|
--| template<class Item, class Value>
--| Value* BC_TMapActiveIterator<Item, Value>::CurrentValue()
--| {
--|   return IsDone() ? 0 : &((Value&)(fMap.ValueAt(fBucketIndex, fIndex)));
--| }
--|
--| template<class Item, class Value>
--| BC_TMapPassiveIterator<Item, Value>::
--|   BC_TMapPassiveIterator(const BC_TMap<Item, Value>& m)
--|     : fMap(m) {}
--|
--| template<class Item, class Value>
--| BC_TMapPassiveIterator<Item, Value>::~BC_TMapPassiveIterator() {}
--|
--| template<class Item, class Value>
--| BC_Boolean BC_TMapPassiveIterator<Item, Value>::
--|   Apply(BC_Boolean (*Process)(const Item&, const Value&))
--| {
--|   BC_TMapActiveIterator<Item, Value> iter(fMap);
--|   while (!iter.IsDone()) {
--|     if (!Process(*iter.CurrentItem(), *iter.CurrentValue()))
--|       return 0;
--|     else
--|       iter.Next();
--|   }
--|   return 1;
--| }

--    package Address_Conversions
--    is new System.Address_To_Access_Conversions (Map);

--    function New_Iterator (For_The_Map : Map) return Iterator is
--      P : Address_Conversions.Object_Pointer
--         := Address_Conversions.To_Pointer (For_The_Map'Address);
--    begin
--      return Iterator (SP.Create (new Map_Iterator (P)));
--    end New_Iterator;

  procedure Visit is
    Iter : Iterator := New_Iterator (Over_The_Container);
    Map_Iter : Map_Iterator
       renames Map_Iterator (SP.Value (SP.Pointer (Iter)).all);
    Status : Boolean;
  begin
    while not Is_Done (Iter) loop
      Apply (Item_At (Over_The_Container,
		      Map_Iter.Bucket_Index,
		      Map_Iter.Index).all,
             Value_At (Over_The_Container,
		       Map_Iter.Bucket_Index,
		       Map_Iter.Index).all,
             Status);
      exit when not Status;
      Next (Iter);
    end loop;
  end Visit;

  procedure Modify is
    Iter : Iterator := New_Iterator (Over_The_Container);
    Map_Iter : Map_Iterator
       renames Map_Iterator (SP.Value (SP.Pointer (Iter)).all);
    Status : Boolean;
  begin
    while not Is_Done (Iter) loop
      Apply (Item_At (Over_The_Container,
		      Map_Iter.Bucket_Index,
		      Map_Iter.Index).all,
             Value_At (Over_The_Container,
		       Map_Iter.Bucket_Index,
		       Map_Iter.Index).all,
             Status);
      exit when not Status;
      Next (Iter);
    end loop;
  end Modify;

--| template<class Item, class Value>
--| BC_Boolean BC_TMapPassiveIterator<Item, Value>::
--|   Apply(BC_Boolean (*Process)(Item&, Value&))
--| {
--|   BC_TMapActiveIterator<Item, Value> iter(fMap);
--|   while (!iter.IsDone()) {
--|     if (!Process(*iter.CurrentItem(), *iter.CurrentValue()))
--|       return 0;
--|     else
--|       iter.Next();
--|   }
--|   return 1;
--| }

  -- Subprograms to be overridden

--|   virtual void Purge() = 0;

  procedure Purge (M : in out Map) is
  begin
    raise Should_Have_Been_Overridden;
  end Purge;

--|   virtual BC_Boolean Attach(const Item&, const Value&) = 0;

  procedure Attach (M : in out Map; I : Item; V : Value) is
  begin
    raise Should_Have_Been_Overridden;
  end Attach;

--|   virtual BC_Index Cardinality() const = 0;

  function Cardinality (M : Map) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Cardinality;

--|   virtual BC_Index NumberOfBuckets() const = 0;

  function Number_Of_Buckets (M : Map) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Number_Of_Buckets;

--|   virtual BC_Index Length(BC_Index bucket) const = 0;

  function Length (M : Map; Bucket : Positive) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Length;

--|   virtual BC_Boolean Exists(const Item&) const = 0;

  function Exists (M : Map; I : Item) return Boolean is
  begin
    raise Should_Have_Been_Overridden;
    return False;
  end Exists;

--|   virtual const Item& ItemAt(BC_Index bucket, BC_Index index) const = 0;

  function Item_At (M : Map; Bucket, Index : Positive) return Item_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Item_At;

--|   virtual const Value& ValueAt(BC_Index bucket, BC_Index index) const = 0;

  function Value_At (M : Map; Bucket, Index : Positive) return Value_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Value_At;

end BC.Containers.Maps;
