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

with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Containers.Bags.Unbounded is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Bags.Unbounded");

  procedure Clear (B : in out Unbounded_Bag) is
  begin
    Tables.Clear (B.Rep);
  end Clear;

  procedure Add (B : in out Unbounded_Bag; I : Item; Added : out Boolean) is
  begin
    if Tables.Is_Bound (B.Rep, I) then
      Tables.Rebind (B.Rep, I, Tables.Value_Of (B.Rep, I) + 1);
      Added := False;
    else
      Tables.Bind (B.Rep, I, 1);
      Added := True;
    end if;
  end Add;

  procedure Remove (B : in out Unbounded_Bag; I : Item) is
    Count : Positive;
  begin
    Assert (Tables.Is_Bound (B.Rep, I),
	    BC.Not_Found'Identity,
	    "Remove",
	    BSE.Missing);
    Count := Tables.Value_Of (B.Rep, I);
    if Count = 1 then
      Tables.Unbind (B.Rep, I);
    else
      Tables.Rebind (B.Rep, I, Count - 1);
    end if;
  end Remove;

  function Extent (B : Unbounded_Bag) return Natural is
  begin
    return Tables.Extent (B.Rep);
  end Extent;

  function Count (B : Unbounded_Bag; I : Item) return Natural is
  begin
    if not Tables.Is_Bound (B.Rep, I) then
      return 0;
    else
      return Tables.Value_Of (B.Rep, I);
    end if;
  end  Count;

  function Is_Empty (B : Unbounded_Bag) return Boolean is
  begin
    return Tables.Extent (B.Rep) = 0;
  end Is_Empty;

  function Is_Member (B : Unbounded_Bag; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (B.Rep, I);
  end Is_Member;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Unbounded_Bag);

  function New_Iterator (For_The_Bag : Unbounded_Bag) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Bag'Address);
  begin
    return Iterator (SP.Create (new Unbounded_Bag_Iterator (P)));
  end New_Iterator;

  -- Private implementations

  procedure Purge (B : in out Unbounded_Bag) is
  begin
    Tables.Clear (B.Rep);
  end Purge;

  procedure Attach (B : in out Unbounded_Bag; I : Item; C : Positive) is
  begin
    Tables.Bind (B.Rep, I, C);
  end Attach;

  procedure Detach (B : in out Unbounded_Bag; I : Item) is
  begin
    Tables.Unbind (B.Rep, I);
  end Detach;

  procedure Set_Value (B : in out Unbounded_Bag; I : Item; C : Positive) is
  begin
    Tables.Rebind (B.Rep, I, C);
  end Set_Value;

  function Cardinality (B : Unbounded_Bag) return Natural is
  begin
    return Tables.Extent (B.Rep);
  end Cardinality;

  function Number_Of_Buckets (B : Unbounded_Bag) return Natural is
  begin
    return Buckets;
  end Number_Of_Buckets;

  function Length (B : Unbounded_Bag; Bucket : Positive) return Natural is
  begin
    return IC.Length (Tables.Item_Bucket (B.Rep, Bucket).all);
  end Length;

  function Exists (B : Unbounded_Bag; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (B.Rep, I);
  end Exists;

  function Value_Of (B : Unbounded_Bag; I : Item) return Positive is
  begin
    raise Program_Error;
    return 1;
  end Value_Of;

  function Item_At
     (B : Unbounded_Bag; Bucket, Index : Positive) return Item_Ptr is
  begin
    return IC.Item_At (Tables.Item_Bucket (B.Rep, Bucket).all, Index);
  end Item_At;

  function Value_At
     (B : Unbounded_Bag; Bucket, Index : Positive) return Positive is
  begin
    return VC.Item_At (Tables.Value_Bucket (B.Rep, Bucket).all, Index);
  end Value_At;

end BC.Containers.Bags.Unbounded;
