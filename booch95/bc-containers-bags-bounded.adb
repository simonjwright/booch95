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

package body BC.Containers.Bags.Bounded is

   package BSE renames BC.Support.Exceptions;
   procedure Assert
   is new BSE.Assert ("BC.Containers.Bags.Bounded");

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

   function Available (B : Unconstrained_Bag) return Natural is
   begin
      return Maximum_Size - B.Rep.Size;
   end Available;

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
      Result : Bounded_Bag_Iterator;
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

   --  Null containers

   Empty_Container : Bag;
   pragma Warnings (Off, Empty_Container);

   function Null_Container return Unconstrained_Bag is
   begin
      return Empty_Container;
   end Null_Container;

   --  Iterators

   procedure Reset (It : in out Bounded_Bag_Iterator) is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      Tables.Reset (S.Rep, It.Bucket_Index, It.Index);
   end Reset;

   procedure Next (It : in out Bounded_Bag_Iterator) is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      Tables.Next (S.Rep, It.Bucket_Index, It.Index);
   end Next;

   function Is_Done (It : Bounded_Bag_Iterator) return Boolean is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      return Tables.Is_Done (S.Rep, It.Bucket_Index, It.Index);
   end Is_Done;

   function Current_Item (It : Bounded_Bag_Iterator) return Item is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Item_Ptr (S.Rep, It.Bucket_Index, It.Index).all;
   end Current_Item;

   function Current_Item_Ptr (It : Bounded_Bag_Iterator) return Item_Ptr is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Item_Ptr (S.Rep, It.Bucket_Index, It.Index);
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Bounded_Bag_Iterator) is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      Tables.Delete_Item_At (S.Rep, It.Bucket_Index, It.Index);
   end Delete_Item_At;

end BC.Containers.Bags.Bounded;
