--  Copyright (C) 1994-2002 Grady Booch and Simon Wright.
--  All Rights Reserved.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Containers.Bags.Dynamic is

   package BSE renames BC.Support.Exceptions;
   procedure Assert
   is new BSE.Assert ("BC.Containers.Bags.Dynamic");

   procedure Clear (B : in out Bag) is
   begin
      Tables.Clear (B.Rep);
   end Clear;

   procedure Add (B : in out Bag; I : Item; Added : out Boolean) is
   begin
      if Tables.Is_Bound (B.Rep, I) then
         Tables.Rebind (B.Rep, I, Tables.Value_Of (B.Rep, I) + 1);
         Added := False;
      else
         Tables.Bind (B.Rep, I, 1);
         Added := True;
      end if;
   end Add;

   procedure Remove (B : in out Bag; I : Item) is
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

   function Extent (B : Bag) return Natural is
   begin
      return Tables.Extent (B.Rep);
   end Extent;

   function Count (B : Bag; I : Item) return Natural is
   begin
      if not Tables.Is_Bound (B.Rep, I) then
         return 0;
      else
         return Tables.Value_Of (B.Rep, I);
      end if;
   end  Count;

   function Is_Empty (B : Bag) return Boolean is
   begin
      return Tables.Extent (B.Rep) = 0;
   end Is_Empty;

   function Is_Member (B : Bag; I : Item) return Boolean is
   begin
      return Tables.Is_Bound (B.Rep, I);
   end Is_Member;

   procedure Preallocate (B : in out Bag; Size : Positive) is
   begin
      for Bucket in 1 .. Buckets loop
         IC.Preallocate (B.Rep.Items (Bucket), Size);
         VC.Preallocate (B.Rep.Values (Bucket), Size);
      end loop;
   end Preallocate;

   procedure Set_Chunk_Size (B : in out Bag; Size : Positive) is
   begin
      for Bucket in 1 .. Buckets loop
         IC.Set_Chunk_Size (B.Rep.Items (Bucket), Size);
         VC.Set_Chunk_Size (B.Rep.Values (Bucket), Size);
      end loop;
   end Set_Chunk_Size;

   function Chunk_Size (B : Bag) return Positive is
   begin
      return IC.Chunk_Size (B.Rep.Items (1));
   end Chunk_Size;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Bag);

   function New_Iterator (For_The_Bag : Bag) return Iterator'Class is
      Result : Bag_Iterator;
   begin
      Result.For_The_Container :=
        Address_Conversions.To_Pointer (For_The_Bag'Address).all'Access;
      Reset (Result);
      return Result;
   end New_Iterator;

   --  Private implementations

   procedure Attach (B : in out Bag; I : Item; C : Positive) is
   begin
      Tables.Bind (B.Rep, I, C);
   end Attach;

   procedure Detach (B : in out Bag; I : Item) is
   begin
      Tables.Unbind (B.Rep, I);
   end Detach;

   procedure Set_Value (B : in out Bag; I : Item; C : Positive) is
   begin
      Tables.Rebind (B.Rep, I, C);
   end Set_Value;

   function Number_Of_Buckets (B : Bag) return Natural is
      pragma Warnings (Off, B);
   begin
      return Buckets;
   end Number_Of_Buckets;

   function Length (B : Bag; Bucket : Positive) return Natural is
   begin
      return IC.Length (B.Rep.Items (Bucket));
   end Length;

   function Item_At (B : Bag; Bucket, Index : Positive) return Item_Ptr is
   begin
      return IC.Item_At (B.Rep.Items (Bucket), Index);
   end Item_At;

   function Value_At (B : Bag; Bucket, Index : Positive) return Positive is
   begin
      return VC.Item_At (B.Rep.Values (Bucket), Index);
   end Value_At;

   Empty_Container : Bag;
   pragma Warnings (Off, Empty_Container);

   function Null_Container return Bag is
   begin
      return Empty_Container;
   end Null_Container;

end BC.Containers.Bags.Dynamic;
