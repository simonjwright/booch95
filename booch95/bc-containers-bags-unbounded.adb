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

package body BC.Containers.Bags.Unbounded is

   package BSE renames BC.Support.Exceptions;
   procedure Assert
   is new BSE.Assert ("BC.Containers.Bags.Unbounded");

   procedure Clear (B : in out Unconstrained_Bag) is
   begin
      Tables.Clear (B.Rep);
   end Clear;

   procedure Add (B : in out Unconstrained_Bag;
                  I : Item;
                  Added : out Boolean) is
   begin
      if Tables.Is_Bound (B.Rep, I) then
         Tables.Rebind (B.Rep, I, Tables.Value_Of (B.Rep, I) + 1);
         Added := False;
      else
         Tables.Bind (B.Rep, I, 1);
         Added := True;
      end if;
   end Add;

   procedure Remove (B : in out Unconstrained_Bag; I : Item) is
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

   function Extent (B : Unconstrained_Bag) return Natural is
   begin
      return Tables.Extent (B.Rep);
   end Extent;

   function Count (B : Unconstrained_Bag; I : Item) return Natural is
   begin
      if not Tables.Is_Bound (B.Rep, I) then
         return 0;
      else
         return Tables.Value_Of (B.Rep, I);
      end if;
   end  Count;

   function Is_Empty (B : Unconstrained_Bag) return Boolean is
   begin
      return Tables.Extent (B.Rep) = 0;
   end Is_Empty;

   function Is_Member (B : Unconstrained_Bag; I : Item) return Boolean is
   begin
      return Tables.Is_Bound (B.Rep, I);
   end Is_Member;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Unconstrained_Bag);

   function New_Iterator
     (For_The_Bag : Unconstrained_Bag) return Iterator'Class is
      Result : Bag_Iterator;
   begin
      Result.For_The_Container :=
        Address_Conversions.To_Pointer (For_The_Bag'Address).all'Access;
      Reset (Result);
      return Result;
   end New_Iterator;

   --  Private implementations

   procedure Attach (B : in out Unconstrained_Bag; I : Item; C : Positive) is
   begin
      Tables.Bind (B.Rep, I, C);
   end Attach;

   procedure Detach (B : in out Unconstrained_Bag; I : Item) is
   begin
      Tables.Unbind (B.Rep, I);
   end Detach;

   procedure Set_Value (B : in out Unconstrained_Bag;
                        I : Item;
                        C : Positive) is
   begin
      Tables.Rebind (B.Rep, I, C);
   end Set_Value;

   function Number_Of_Buckets (B : Unconstrained_Bag) return Natural is
      pragma Warnings (Off, B);
   begin
      return Buckets;
   end Number_Of_Buckets;

   function Length (B : Unconstrained_Bag; Bucket : Positive) return Natural is
   begin
      return IC.Length (B.Rep.Items (Bucket));
   end Length;

   function Item_At (B : Unconstrained_Bag;
                     Bucket, Index : Positive) return Item_Ptr is
   begin
      return IC.Item_At (B.Rep.Items (Bucket), Index);
   end Item_At;

   function Value_At (B : Unconstrained_Bag;
                      Bucket, Index : Positive) return Positive is
   begin
      return VC.Item_At (B.Rep.Values (Bucket), Index);
   end Value_At;

   Empty_Container : Bag;
   pragma Warnings (Off, Null_Container);

   function Null_Container return Unconstrained_Bag is
   begin
      return Empty_Container;
   end Null_Container;

end BC.Containers.Bags.Unbounded;
