-- Copyright (C) 1994-1999 Grady Booch and Simon Wright.
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

package body BC.Containers.Sets.Bounded is

  procedure Clear (S : in out Bounded_Set) is
  begin
    Tables.Clear (S.Rep);
  end Clear;

  procedure Add (S : in out Bounded_Set; I : Item) is
  begin
    Tables.Bind (S.Rep, I, True);
  end Add;

  procedure Remove (S : in out Bounded_Set; I : Item) is
  begin
    Tables.Unbind (S.Rep, I);
  end Remove;

  function Available (S : Bounded_Set) return Natural is
    Count : Natural := 0;
  begin
    for B in 1 .. Buckets loop
      Count := Count + IC.Available (Tables.Item_Bucket (S.Rep, B).all);
    end loop;
    return Count;
  end Available;

  function Extent (S : Bounded_Set) return Natural is
  begin
    return Tables.Extent (S.Rep);
  end Extent;

  function Is_Empty (S : Bounded_Set) return Boolean is
  begin
    return Tables.Extent (S.Rep) = 0;
  end Is_Empty;

  function Is_Member (S : Bounded_Set; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (S.Rep, I);
  end Is_Member;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Bounded_Set);

  function New_Iterator (For_The_Set : Bounded_Set) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Set'Address);
  begin
    return Iterator (SP.Create (new Bounded_Set_Iterator (P)));
  end New_Iterator;

  -- Private implementations

  procedure Purge (S : in out Bounded_Set) is
  begin
    Tables.Clear (S.Rep);
  end Purge;

  -- XXX there is another Attach() which I don't understand

  procedure Attach (S : in out Bounded_Set; I : Item) is
  begin
    Tables.Bind (S.Rep, I, True);
  end Attach;

  procedure Detach (S : in out Bounded_Set; I : Item) is
  begin
    Tables.Unbind (S.Rep, I);
  end Detach;

  function Cardinality (S : Bounded_Set) return Natural is
  begin
    return Tables.Extent (S.Rep);
  end Cardinality;

  function Number_Of_Buckets (S : Bounded_Set) return Natural is
  begin
    return Buckets;
  end Number_Of_Buckets;

  function Length (S : Bounded_Set; Bucket : Positive) return Natural is
  begin
    return IC.Length (Tables.Item_Bucket (S.Rep, Bucket).all);
  end Length;

  function Exists (S : Bounded_Set; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (S.Rep, I);
  end Exists;

  function Item_At
     (S : Bounded_Set; Bucket, Index : Positive) return Item_Ptr is
  begin
    return IC.Item_At (Tables.Item_Bucket (S.Rep, Bucket).all, Index);
  end Item_At;

end BC.Containers.Sets.Bounded;
