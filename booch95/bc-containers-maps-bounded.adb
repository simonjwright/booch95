-- Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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

  procedure Clear (M : in out Bounded_Map) is
  begin
    Tables.Clear (M.Rep);
  end Clear;

  procedure Bind
     (M : in out Bounded_Map; K : Key; I : Item) is
  begin
    Tables.Bind (M.Rep, K, I);
  end Bind;

  procedure Rebind
     (M : in out Bounded_Map; K : Key; I : Item) is
  begin
    Tables.Rebind (M.Rep, K, I);
  end Rebind;

  procedure Unbind (M : in out Bounded_Map; K : Key) is
  begin
    Tables.Unbind (M.Rep, K);
  end Unbind;

  function Available (M : Bounded_Map) return Natural is
    Count : Natural := 0;
  begin
    for B in 1 .. Buckets loop
      Count := Count + KC.Available (Tables.Item_Bucket (M.Rep, B).all);
    end loop;
    return Count;
  end Available;

  function Extent (M : Bounded_Map) return Natural is
  begin
    return Tables.Extent (M.Rep);
  end Extent;

  function Is_Empty (M : Bounded_Map) return Boolean is
  begin
    return Tables.Extent (M.Rep) = 0;
  end Is_Empty;

  function Is_Bound (M : Bounded_Map; K : Key) return Boolean is
  begin
    return Tables.Is_Bound (M.Rep, K);
  end Is_Bound;

  function Item_Of (M : Bounded_Map; K : Key) return Item is
  begin
    return Tables.Value_Of (M.Rep, K);
  end Item_Of;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Bounded_Map);

  function New_Iterator (For_The_Map : Bounded_Map) return Iterator'Class is
    Result : Map_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Map'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  -- Private implementations

  procedure Attach (M : in out Bounded_Map; K : Key; I : Item) is
  begin
    Tables.Bind (M.Rep, K, I);
  end Attach;

  function Number_Of_Buckets (M : Bounded_Map) return Natural is
  begin
    return Buckets;
  end Number_Of_Buckets;

  function Length (M : Bounded_Map; Bucket : Positive) return Natural is
  begin
    return KC.Length (Tables.Item_Bucket (M.Rep, Bucket).all);
  end Length;

  function Item_At
     (M : Bounded_Map; Bucket, Index : Positive) return Item_Ptr is
  begin
    return IC.Item_At (Tables.Value_Bucket (M.Rep, Bucket).all, Index);
  end Item_At;

  function Key_At
     (M : Bounded_Map; Bucket, Index : Positive) return Key_Ptr is
  begin
    return KC.Item_At (Tables.Item_Bucket (M.Rep, Bucket).all, Index);
  end Key_At;

  Empty_Container : Bounded_Map;

  function Null_Container return Bounded_Map is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Maps.Bounded;
