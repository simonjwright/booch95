--  Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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

--  $Id$

with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Containers.Bags.Bounded is

   package BSE renames BC.Support.Exceptions;
   procedure Assert
   is new BSE.Assert ("BC.Containers.Bags.Bounded");

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

   function Available (B : Bag) return Natural is
   begin
      return Maximum_Size - B.Rep.Size;
   end Available;

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

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Bag);

   function New_Iterator (For_The_Bag : Bag) return Iterator'Class is
      Result : Bounded_Bag_Iterator;
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
   begin
      return Buckets;
   end Number_Of_Buckets;

   function Item_At (B : Bag; Bucket, Index : Positive) return Item_Ptr is
   begin
      return Tables.Access_Item_At (B.Rep, Index);
   end Item_At;

   function Value_At (B : Bag; Bucket, Index : Positive) return Positive is
   begin
      return B.Rep.Contents (Index).Value;
   end Value_At;

   procedure Reset (It : in out Bounded_Bag_Iterator) is
      B : Bag'Class renames Bag'Class (It.For_The_Container.all);
   begin
      It.Index := 0;
      if Extent (B) = 0 then
         It.Bucket_Index := 0;
      else
         It.Bucket_Index := 1;
         while It.Bucket_Index <= Number_Of_Buckets (B) loop
            if B.Rep.Buckets (It.Bucket_Index) > 0 then
               It.Index := B.Rep.Buckets (It.Bucket_Index);
               exit;
            end if;
            It.Bucket_Index := It.Bucket_Index + 1;
         end loop;
      end if;
   end Reset;

   procedure Next (It : in out Bounded_Bag_Iterator) is
      B : Bag'Class renames Bag'Class (It.For_The_Container.all);
   begin
      if It.Bucket_Index <= Number_Of_Buckets (B) then
         if B.Rep.Contents (It.Index).Next > 0 then
            It.Index := B.Rep.Contents (It.Index).Next;
         else
            It.Bucket_Index := It.Bucket_Index + 1;
            It.Index := 0;
            while It.Bucket_Index <= Number_Of_Buckets (B) loop
               if B.Rep.Buckets (It.Bucket_Index) > 0 then
                  It.Index := B.Rep.Buckets (It.Bucket_Index);
                  exit;
               end if;
               It.Bucket_Index := It.Bucket_Index + 1;
            end loop;
         end if;
      end if;
   end Next;

   function Is_Done (It : Bounded_Bag_Iterator) return Boolean is
      B : Bag'Class renames Bag'Class (It.For_The_Container.all);
   begin
      if It.Bucket_Index = 0
        or else It.Bucket_Index > Number_Of_Buckets (B) then
         return True;
      end if;
      if It.Index > 0 then
         return False;
      end if;
      declare
         package Conversions is new System.Address_To_Access_Conversions
           (Bounded_Bag_Iterator'Class);
         P : Conversions.Object_Pointer := Conversions.To_Pointer (It'Address);
      begin
         P.Bucket_Index := P.Bucket_Index + 1;
         P.Index := 0;
         while P.Bucket_Index <= Number_Of_Buckets (B) loop
            if B.Rep.Buckets (P.Bucket_Index) > 0 then
               P.Index := B.Rep.Buckets (P.Bucket_Index);
               return False;
            end if;
            P.Bucket_Index := P.Bucket_Index + 1;
         end loop;
      end;
      return True;
   end Is_Done;

   function Current_Item (It : Bounded_Bag_Iterator) return Item is
      B : Bag'Class renames Bag'Class (It.For_The_Container.all);
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return B.Rep.Contents (It.Index).Item;
   end Current_Item;

   function Current_Item_Ptr (It : Bounded_Bag_Iterator) return Item_Ptr is
      --  XXX this should probably not be permitted!
      B : Bag'Class renames Bag'Class (It.For_The_Container.all);
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return Tables.Access_Item_At (B.Rep, It.Index);
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Bounded_Bag_Iterator) is
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      raise BC.Not_Yet_Implemented;
   end Delete_Item_At;

   Empty_Container : Bag;
   pragma Warnings (Off, Empty_Container);

   function Null_Container return Bag is
   begin
      return Empty_Container;
   end Null_Container;

end BC.Containers.Bags.Bounded;
