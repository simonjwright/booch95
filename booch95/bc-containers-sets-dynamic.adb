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

package body BC.Containers.Sets.Dynamic is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Containers.Sets.Dynamic");

  function Create (Size : Positive) return Dynamic_Set is
    S : Dynamic_Set;
  begin
    for B in 1 .. Buckets loop
      IC.Set_Chunk_Size (Tables.Item_Bucket (S.Rep, B).all, Size);
      VC.Set_Chunk_Size (Tables.Value_Bucket (S.Rep, B).all, Size);
    end loop;
    return S;
  end Create;

  procedure Clear (S : in out Dynamic_Set) is
  begin
    Tables.Clear (S.Rep);
  end Clear;

  procedure Add (S : in out Dynamic_Set; I : Item; Added : out Boolean) is
  begin
    if Tables.Is_Bound (S.Rep, I) then
      Added := False;
    else
      Tables.Bind (S.Rep, I, True);
      Added := True;
    end if;
  end Add;

  procedure Remove (S : in out Dynamic_Set; I : Item) is
  begin
    Assert (Tables.Is_Bound (S.Rep, I),
            BC.Not_Found'Identity,
            "Remove",
            BSE.Missing);
    Tables.Unbind (S.Rep, I);
  end Remove;

  function Extent (S : Dynamic_Set) return Natural is
  begin
    return Tables.Extent (S.Rep);
  end Extent;

  function Is_Empty (S : Dynamic_Set) return Boolean is
  begin
    return Tables.Extent (S.Rep) = 0;
  end Is_Empty;

  function Is_Member (S : Dynamic_Set; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (S.Rep, I);
  end Is_Member;

  procedure Preallocate (S : in out Dynamic_Set; Size : Positive) is
  begin
    for B in 1 .. Buckets loop
      IC.Preallocate (Tables.Item_Bucket (S.Rep, B).all, Size);
      VC.Preallocate (Tables.Value_Bucket (S.Rep, B).all, Size);
    end loop;
  end Preallocate;

  procedure Set_Chunk_Size (S : in out Dynamic_Set; Size : Positive) is
  begin
    for B in 1 .. Buckets loop
      IC.Set_Chunk_Size (Tables.Item_Bucket (S.Rep, B).all, Size);
      VC.Set_Chunk_Size (Tables.Value_Bucket (S.Rep, B).all, Size);
    end loop;
  end Set_Chunk_Size;

  function Chunk_Size (S : Dynamic_Set) return Positive is
  begin
    return IC.Chunk_Size (Tables.Item_Bucket (S.Rep, 1).all);
  end Chunk_Size;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Dynamic_Set);

  function New_Iterator (For_The_Set : Dynamic_Set) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Set'Address);
  begin
    return Iterator (SP.Create (new Dynamic_Set_Iterator (P)));
  end New_Iterator;

  -- Private implementations

  procedure Purge (S : in out Dynamic_Set) is
  begin
    Tables.Clear (S.Rep);
  end Purge;

  procedure Attach (S : in out Dynamic_Set; I : Item) is
  begin
    Tables.Bind (S.Rep, I, True);
  end Attach;

  procedure Detach (S : in out Dynamic_Set; I : Item) is
  begin
    Tables.Unbind (S.Rep, I);
  end Detach;

  function Cardinality (S : Dynamic_Set) return Natural is
  begin
    return Tables.Extent (S.Rep);
  end Cardinality;

  function Number_Of_Buckets (S : Dynamic_Set) return Natural is
  begin
    return Buckets;
  end Number_Of_Buckets;

  function Length (S : Dynamic_Set; Bucket : Positive) return Natural is
  begin
    return IC.Length (Tables.Item_Bucket (S.Rep, Bucket).all);
  end Length;

  function Exists (S : Dynamic_Set; I : Item) return Boolean is
  begin
    return Tables.Is_Bound (S.Rep, I);
  end Exists;

  function Item_At
     (S : Dynamic_Set; Bucket, Index : Positive) return Item_Ptr is
  begin
    return IC.Item_At (Tables.Item_Bucket (S.Rep, Bucket).all, Index);
  end Item_At;

end BC.Containers.Sets.Dynamic;
