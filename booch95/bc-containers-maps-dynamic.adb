-- Copyright (C) 1994-2000 Grady Booch and Simon Wright.
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

  function New_Iterator (For_The_Map : Dynamic_Map) return Iterator'Class is
    Result : Map_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Map'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  -- Private implementations

  procedure Attach (M : in out Dynamic_Map; I : Item; V : Value) is
  begin
    Tables.Bind (M.Rep, I, V);
  end Attach;

  function Number_Of_Buckets (M : Dynamic_Map) return Natural is
  begin
    return Buckets;
  end Number_Of_Buckets;

  function Length (M : Dynamic_Map; Bucket : Positive) return Natural is
  begin
    return IC.Length (Tables.Item_Bucket (M.Rep, Bucket).all);
  end Length;

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

end BC.Containers.Maps.Dynamic;
