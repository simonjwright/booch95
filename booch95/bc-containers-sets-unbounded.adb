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

with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Containers.Sets.Unbounded is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Sets.Unbounded");

  procedure Clear (S : in out Set) is
  begin
    Tables.Clear (S.Rep);
  end Clear;

  procedure Add (S : in out Set; I : Item; Added : out Boolean) is
  begin
    if Tables.Is_Bound (S.Rep, I) then
      Added := False;
    else
      Tables.Bind (S.Rep, I, True);
      Added := True;
    end if;
  end Add;

  procedure Remove (S : in out Set; I : Item) is
  begin
    Assert (Tables.Is_Bound (S.Rep, I),
            BC.Not_Found'Identity,
            "Remove",
            BSE.Missing);
    Tables.Unbind (S.Rep, I);
  end Remove;

  function Extent (S : Set) return Natural is
  begin
    return Tables.Extent (S.Rep);
  end Extent;

  function Is_Empty (S : Set) return Boolean is
  begin
    return Tables.Extent (S.Rep) = 0;
  end Is_Empty;

  function Is_Member (S : Set; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (S.Rep, I);
  end Is_Member;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Set);

  function New_Iterator (For_The_Set : Set) return Iterator'Class is
    Result : Set_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Set'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  -- Private implementations

  procedure Attach (S : in out Set; I : Item) is
  begin
    Tables.Bind (S.Rep, I, True);
  end Attach;

  procedure Detach (S : in out Set; I : Item) is
  begin
    Tables.Unbind (S.Rep, I);
  end Detach;

  function Number_Of_Buckets (S : Set) return Natural is
  begin
    return Buckets;
  end Number_Of_Buckets;

  function Length (S : Set; Bucket : Positive) return Natural is
  begin
    return IC.Length (S.Rep.Items (Bucket));
  end Length;

  function Item_At (S : Set; Bucket, Index : Positive) return Item_Ptr is
  begin
    return IC.Item_At (S.Rep.Items (Bucket), Index);
  end Item_At;

  Empty_Container : Set;
  pragma Warnings (Off, Empty_Container);

  function Null_Container return Set is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Sets.Unbounded;
