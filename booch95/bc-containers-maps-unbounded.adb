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

package body BC.Containers.Maps.Unbounded is

  procedure Clear (M : in out Unbounded_Map) is
  begin
    Tables.Clear (M.Rep);
  end Clear;

  procedure Bind
     (M : in out Unbounded_Map; I : Item; V : Value) is
  begin
    Tables.Bind (M.Rep, I, V);
  end Bind;

  procedure Rebind
     (M : in out Unbounded_Map; I : Item; V : Value) is
  begin
    Tables.Rebind (M.Rep, I, V);
  end Rebind;

  procedure Unbind (M : in out Unbounded_Map; I : Item) is
  begin
    Tables.Unbind (M.Rep, I);
  end Unbind;

  function Extent (M : Unbounded_Map) return Natural is
  begin
    return Tables.Extent (M.Rep);
  end Extent;

  function Is_Empty (M : Unbounded_Map) return Boolean is
  begin
    return Tables.Extent (M.Rep) = 0;
  end Is_Empty;

  function Is_Bound (M : Unbounded_Map; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (M.Rep, I);
  end Is_Bound;

  function Value_Of (M : Unbounded_Map; I : Item) return Value is
  begin
    return Tables.Value_Of (M.Rep, I);
  end Value_Of;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Unbounded_Map);

  function New_Iterator (For_The_Map : Unbounded_Map) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Map'Address);
  begin
    return Iterator (SP.Create (new Unbounded_Map_Iterator (P)));
  end New_Iterator;

  -- Private implementations

  procedure Purge (M : in out Unbounded_Map) is
  begin
    Tables.Clear (M.Rep);
  end Purge;

  procedure Attach (M : in out Unbounded_Map; I : Item; V : Value) is
  begin
    Tables.Bind (M.Rep, I, V);
  end Attach;

  function Cardinality (M : Unbounded_Map) return Natural is
  begin
    return Tables.Extent (M.Rep);
  end Cardinality;

  function Number_Of_Buckets (M : Unbounded_Map) return Natural is
  begin
    return Buckets;
  end Number_Of_Buckets;

  function Length (M : Unbounded_Map; Bucket : Positive) return Natural is
  begin
    return IC.Length (Tables.Item_Bucket (M.Rep, Bucket).all);
  end Length;

  function Exists (M : Unbounded_Map; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (M.Rep, I);
  end Exists;

  function Item_At
     (M : Unbounded_Map; Bucket, Index : Positive) return Item_Ptr is
  begin
    return IC.Item_At (Tables.Item_Bucket (M.Rep, Bucket).all, Index);
  end Item_At;

  function Value_At
     (M : Unbounded_Map; Bucket, Index : Positive) return Value_Ptr is
  begin
    return VC.Item_At (Tables.Value_Bucket (M.Rep, Bucket).all, Index);
  end Value_At;

end BC.Containers.Maps.Unbounded;
