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

with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Containers.Bags.Dynamic is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Bags.Dynamic");

  function Create (Size : Positive) return Dynamic_Bag is
    Result : Dynamic_Bag;
  begin
    for B in 1 .. Buckets loop
      IC.Set_Chunk_Size (Tables.Item_Bucket (Result.Rep, B).all, Size);
      VC.Set_Chunk_Size (Tables.Value_Bucket (Result.Rep, B).all, Size);
    end loop;
    return Result;
  end Create;

  procedure Clear (B : in out Dynamic_Bag) is
  begin
    Tables.Clear (B.Rep);
  end Clear;

  procedure Add (B : in out Dynamic_Bag; I : Item; Added : out Boolean) is
  begin
    if Tables.Is_Bound (B.Rep, I) then
      Tables.Rebind (B.Rep, I, Tables.Value_Of (B.Rep, I) + 1);
      Added := False;
    else
      Tables.Bind (B.Rep, I, 1);
      Added := True;
    end if;
  end Add;

  procedure Remove (B : in out Dynamic_Bag; I : Item) is
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

  function Extent (B : Dynamic_Bag) return Natural is
  begin
    return Tables.Extent (B.Rep);
  end Extent;

  function Count (B : Dynamic_Bag; I : Item) return Natural is
  begin
    if not Tables.Is_Bound (B.Rep, I) then
      return 0;
    else
      return Tables.Value_Of (B.Rep, I);
    end if;
  end  Count;

  function Is_Empty (B : Dynamic_Bag) return Boolean is
  begin
    return Tables.Extent (B.Rep) = 0;
  end Is_Empty;

  function Is_Member (B : Dynamic_Bag; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (B.Rep, I);
  end Is_Member;

  procedure Preallocate (B : in out Dynamic_Bag; Size : Positive) is
  begin
    for Bucket in 1 .. Buckets loop
      IC.Preallocate (Tables.Item_Bucket (B.Rep, Bucket).all, Size);
      VC.Preallocate (Tables.Value_Bucket (B.Rep,Bucket ).all, Size);
    end loop;
  end Preallocate;

  procedure Set_Chunk_Size (B : in out Dynamic_Bag; Size : Positive) is
  begin
    for Bucket in 1 .. Buckets loop
      IC.Set_Chunk_Size (Tables.Item_Bucket (B.Rep, Bucket).all, Size);
      VC.Set_Chunk_Size (Tables.Value_Bucket (B.Rep, Bucket).all, Size);
    end loop;
  end Set_Chunk_Size;

  function Chunk_Size (B : Dynamic_Bag) return Positive is
  begin
    return IC.Chunk_Size (Tables.Item_Bucket (B.Rep, 1).all);
  end Chunk_Size;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Dynamic_Bag);

  function New_Iterator
     (For_The_Bag : Dynamic_Bag) return Iterator'Class is
    Result : Bag_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Bag'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  -- Private implementations

  procedure Attach (B : in out Dynamic_Bag; I : Item; C : Positive) is
  begin
    Tables.Bind (B.Rep, I, C);
  end Attach;

  procedure Detach (B : in out Dynamic_Bag; I : Item) is
  begin
    Tables.Unbind (B.Rep, I);
  end Detach;

  procedure Set_Value (B : in out Dynamic_Bag; I : Item; C : Positive) is
  begin
    Tables.Rebind (B.Rep, I, C);
  end Set_Value;

  function Number_Of_Buckets (B : Dynamic_Bag) return Natural is
  begin
    return Buckets;
  end Number_Of_Buckets;

  function Length (B : Dynamic_Bag; Bucket : Positive) return Natural is
  begin
    return IC.Length (Tables.Item_Bucket (B.Rep, Bucket).all);
  end Length;

  function Item_At
     (B : Dynamic_Bag; Bucket, Index : Positive) return Item_Ptr is
  begin
    return IC.Item_At (Tables.Item_Bucket (B.Rep, Bucket).all, Index);
  end Item_At;

  function Value_At
     (B : Dynamic_Bag; Bucket, Index : Positive) return Positive is
  begin
    return VC.Item_At (Tables.Value_Bucket (B.Rep, Bucket).all, Index);
  end Value_At;

end BC.Containers.Bags.Dynamic;
