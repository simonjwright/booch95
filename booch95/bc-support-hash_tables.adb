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

package body BC.Support.Hash_Tables is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Support.Hash_Tables");

--| //  The C++ Booch Components (Version 2.3)
--| //  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--| //
--| //  Restricted Rights Legend
--| //  Use, duplication, or disclosure is subject to restrictions as set forth
--| //  in subdivision (c)(1)(ii) of the Rights in Technical Data and Computer
--| //  Software clause at DFARS 252.227-7013.
--| //
--| //  BCHashTa.cpp
--| //
--| //  This file contains the definition for the open hash table class.
--|
--| #include "BCHashTa.h"
--|
--| template<class Item, class Value, BC_Index Buckets,
--|          class ItemContainer, class ValueContainer>
--| BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>::
--|   BC_TTable(const BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>& t)
--|     : fSize(0),
--|       fHash(t.fHash)
--| {
--|   operator=(t);
--| }
--|
--| template<class Item, class Value, BC_Index Buckets,
--|          class ItemContainer, class ValueContainer>
--| BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>&
--|   BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>::
--|     operator=(const BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>& t)
--| {
--|   if (this == &t)
--|     return *this;
--|   else {
--|     Clear();
--|     fHash = t.fHash;
--|     BC_Assert((fHash != 0), BC_XIsNull("BC_TTable::operator=", BC_kNull));
--|     for (BC_Index bucketIndex = 0; (bucketIndex < Buckets); bucketIndex++)
--|       for (BC_Index index = 0; (index < t.fItemRep[bucketIndex].Length()); index++) {
--|         BC_Index bucket = fHash(t.fItemRep[bucketIndex][index]) % Buckets;
--|         fItemRep[bucket].Append(t.fItemRep[bucketIndex][index]);
--|         fValueRep[bucket].Append(t.fValueRep[bucketIndex][index]);
--|       }
--|     fSize = t.fSize;
--|     return *this;
--|   }
--| }

--| template<class Item, class Value, BC_Index Buckets,
--|          class ItemContainer, class ValueContainer>
--| BC_Boolean BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>::operator==
--|   (const BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>& t) const
--| {
--|   if (this == &t)
--|     return 1;
--|   else {
--|     if (fSize == t.fSize) {
--|       for (BC_Index bucket = 0; (bucket < Buckets); bucket++)
--|         for (BC_Index index = 0; (index < fItemRep[bucket].Length()); index++) {
--|           const Item& item = fItemRep[bucket].ItemAt(index);
--|           const Value& value = fValueRep[bucket].ItemAt(index);
--|           if (!t.IsBound(item) || (value != *(t.ValueOf(item))))
--|             return 0;
--|       }
--|       return 1;
--|     }
--|     return 0;
--|   }
--| }

  function "=" (L, R : Table) return Boolean is
  begin
    -- optimisation if L, R are the same Table?
    if L.Size = R.Size then
      for B in 1 .. Buckets loop
        for Index in 1 .. Length (L.Items (B)) loop
          declare
            This_Item : Item renames Item_At (L.Items (B), Index);
          begin
            if not Is_Bound (R, This_Item)
               or else not (Item_At (L.Values (B), Index)
                            = Value'(Value_Of (R, This_Item))) then
              return False;
            end if;
          end;
        end loop;
      end loop;
      return True;
    else
      return False;
    end if;
  end "=";

--| template<class Item, class Value, BC_Index Buckets,
--|          class ItemContainer, class ValueContainer>
--| void BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>::Clear()
--| {
--|   for (BC_Index index = 0; (index < Buckets); index++) {
--|     fItemRep[index].Clear();
--|     fValueRep[index].Clear();
--|   }
--|   fSize = 0;
--| }

  procedure Clear (T : in out Table) is
  begin
    for B in 1 .. Buckets loop
      Clear (T.Items (B));
      Clear (T.Values (B));
      T.Size := 0;
    end loop;
  end Clear;

--| template<class Item, class Value, BC_Index Buckets,
--|          class ItemContainer, class ValueContainer>
--| BC_Boolean BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>::
--|   Bind(const Item& item, const Value& value)
--| {
--|   BC_Assert((fHash != 0), BC_XIsNull("BC_TTable::Bind", BC_kNull));
--|   BC_Index bucket = fHash(item) % Buckets;
--|   BC_ExtendedIndex index = fItemRep[bucket].Location(item);
--|   if (index != -1)
--|     return 0;
--|   else {
--|     fItemRep[bucket].Insert(item);
--|     fValueRep[bucket].Insert(value);
--|     fSize++;
--|   }
--|   return 1;
--| }

  procedure Bind (T : in out Table; I : Item; V : Value) is
    Bucket : constant Positive := (Hash (I) mod Buckets) + 1;
  begin
    Assert (Location (T.Items (Bucket), I, 1) = 0,
            BC.Duplicate'Identity,
            "Bind",
            BSE.Duplicate);
    Insert (T.Items (Bucket), I);
    Insert (T.Values (Bucket), V);
    T.Size := T.Size + 1;
  end Bind;

--| template<class Item, class Value, BC_Index Buckets,
--|          class ItemContainer, class ValueContainer>
--| BC_Boolean BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>::
--|   Rebind(const Item& item, const Value& value)
--| {
--|   BC_Assert((fHash != 0), BC_XIsNull("BC_TTable::Rebind", BC_kNull));
--|   BC_Index bucket = fHash(item) % Buckets;
--|   BC_ExtendedIndex index = fItemRep[bucket].Location(item);
--|   if (index != -1) {
--|     fValueRep[bucket].Replace(index, value);
--|     return 1;
--|    }
--|    return 0;
--| }

  procedure Rebind (T : in out Table; I : Item; V : Value) is
    Bucket : constant Positive := (Hash (I) mod Buckets) + 1;
    Index : constant Natural := Location (T.Items (Bucket), I, 1);
  begin
    Assert (Index /= 0,
            BC.Not_Found'Identity,
            "Rebind",
            BSE.Missing);
    Replace (T.Values (Bucket), Index, V);
  end Rebind;

--| template<class Item, class Value, BC_Index Buckets,
--|          class ItemContainer, class ValueContainer>
--| BC_Boolean BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>::
--|   Unbind(const Item& item)
--| {
--|   BC_Assert((fHash != 0), BC_XIsNull("BC_TTable::Unbind", BC_kNull));
--|   BC_Index bucket = fHash(item) % Buckets;
--|   BC_ExtendedIndex index = fItemRep[bucket].Location(item);
--|   if (index != -1) {
--|     fItemRep[bucket].Remove(index);
--|     fValueRep[bucket].Remove(index);
--|     fSize--;
--|     return 1;
--|   }
--|   return 0;
--| }

  procedure Unbind (T : in out Table; I : Item) is
    Bucket : constant Positive := (Hash (I) mod Buckets) + 1;
    Index : constant Natural := Location (T.Items (Bucket), I, 1);
  begin
    Assert (Index /= 0,
            BC.Not_Found'Identity,
            "Unbind",
            BSE.Missing);
    Remove (T.Items (Bucket), Index);
    Remove (T.Values (Bucket), Index);
    T.Size := T.Size - 1;
  end Unbind;

  function Extent (T : Table) return Natural is
  begin
    return T.Size;
  end Extent;

--| template<class Item, class Value, BC_Index Buckets,
--|          class ItemContainer, class ValueContainer>
--| BC_Boolean BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>::
--|   IsBound(const Item& item) const
--| {
--|   BC_Assert((fHash != 0), BC_XIsNull("BC_TTable::IsBound", BC_kNull));
--|   BC_Index bucket =
--|     ((BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>&)*this).
--|       fHash(item) % Buckets;
--|   BC_ExtendedIndex index = fItemRep[bucket].Location(item);
--|   return (index != -1);
--| }

  function Is_Bound (T : Table; I : Item) return Boolean is
    Bucket : constant Positive := (Hash (I) mod Buckets) + 1;
  begin
    return Location (T.Items (Bucket), I, 1) /= 0;
  end Is_Bound;

--| template<class Item, class Value, BC_Index Buckets,
--|          class ItemContainer, class ValueContainer>
--| const Value* BC_TTable<Item, Value,Buckets, ItemContainer, ValueContainer>::
--|   ValueOf(const Item& item) const
--| {
--|   BC_Assert((fHash != 0), BC_XIsNull("BC_TTable::ValueOf", BC_kNull));
--|   BC_Index bucket =
--|     ((BC_TTable<Item, Value, Buckets, ItemContainer, ValueContainer>&)*this).
--|       fHash(item) % Buckets;
--|   BC_ExtendedIndex index = fItemRep[bucket].Location(item);
--|   return (index != -1) ? &fValueRep[bucket][index] : 0;
--| }

  function Value_Of (T : Table; I : Item) return Value is
    Bucket : constant Positive := (Hash (I) mod Buckets) + 1;
    Index : constant Natural := Location (T.Items (Bucket), I, 1);
  begin
    Assert (Index /= 0,
            BC.Not_Found'Identity,
            "Value_Of",
            BSE.Missing);
    return Item_At (T.Values (Bucket), Index);
  end Value_Of;

  function Value_Of (T : Table; I : Item) return Value_Ptr is
    Bucket : constant Positive := (Hash (I) mod Buckets) + 1;
    Index : constant Natural := Location (T.Items (Bucket), I, 1);
  begin
    Assert (Index /= 0,
            BC.Not_Found'Identity,
            "Value_Of",
            BSE.Missing);
    return Item_At (T.Values (Bucket), Index);
  end Value_Of;

--|   const ItemContainer *const ItemBucket(BC_Index bucket) const
--|     {BC_Assert((bucket < Buckets),
--|                BC_XRangeError("BC_TTable::ItemBucket", BC_kInvalidIndex));
--|      return &fItemRep[bucket];}

  package Item_Conversions is new System.Address_To_Access_Conversions
     (Item_Container);
  -- This needs to be declared at this level rather than locally to the
  -- place of use, to avoid access-level problems (in GNAT)

  function Item_Bucket
     (T : Table; Bucket : Positive) return Item_Container_Ptr is
  begin
    Assert (Bucket <= Buckets,
            BC.Container_Error'Identity,
            "Item_Bucket");
    declare
      P : Item_Conversions.Object_Pointer
         := Item_Conversions.To_Pointer (T.Items (Bucket)'Address);
    begin
      return P.all'Access;
    end;
  end Item_Bucket;

--|   const ValueContainer *const ValueBucket(BC_Index bucket) const
--|     {BC_Assert((bucket < Buckets),
--|                BC_XRangeError("BC_TTable::ValueBucket", BC_kInvalidIndex));
--|      return &fValueRep[bucket];}

  package Value_Conversions is new System.Address_To_Access_Conversions
     (Value_Container);

  function Value_Bucket
     (T : Table; Bucket : Positive) return Value_Container_Ptr is
  begin
    Assert (Bucket <= Buckets,
            BC.Container_Error'Identity,
            "Value_Bucket");
    declare
      P : Value_Conversions.Object_Pointer
         := Value_Conversions.To_Pointer (T.Values (Bucket)'Address);
    begin
      return P.all'Access;
    end;
  end Value_Bucket;

  procedure Finalize (T : in out Table) is
  begin
    Clear (T);
  end Finalize;

end BC.Support.Hash_Tables;
