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

package body BC.Containers.Sets.Bounded is

   package BSE renames BC.Support.Exceptions;
   procedure Assert
   is new BSE.Assert ("BC.Containers.Sets.Bounded");

   procedure Clear (S : in out Unconstrained_Set) is
   begin
      Tables.Clear (S.Rep);
   end Clear;

   procedure Add (S : in out Unconstrained_Set;
                  I : Item;
                  Added : out Boolean) is
   begin
      if Tables.Is_Bound (S.Rep, I) then
         Added := False;
      else
         Tables.Bind (S.Rep, I, True);
         Added := True;
      end if;
   end Add;

   procedure Add (S : in out Unconstrained_Set; I : Item) is
   begin
      if not Tables.Is_Bound (S.Rep, I) then
         Tables.Bind (S.Rep, I, True);
      end if;
   end Add;

   procedure Remove (S : in out Unconstrained_Set; I : Item) is
   begin
      Assert (Tables.Is_Bound (S.Rep, I),
              BC.Not_Found'Identity,
              "Remove",
              BSE.Missing);
      Tables.Unbind (S.Rep, I);
   end Remove;

   function Available (S : Unconstrained_Set) return Natural is
   begin
      return Maximum_Size - S.Rep.Size;
   end Available;

   function Extent (S : Unconstrained_Set) return Natural is
   begin
      return Tables.Extent (S.Rep);
   end Extent;

   function Is_Empty (S : Unconstrained_Set) return Boolean is
   begin
      return Tables.Extent (S.Rep) = 0;
   end Is_Empty;

   function Is_Member (S : Unconstrained_Set; I : Item) return Boolean is
   begin
      return Tables.Is_Bound (S.Rep, I);
   end Is_Member;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Unconstrained_Set);

   function New_Iterator
     (For_The_Set : Unconstrained_Set) return Iterator'Class is
      Result : Bounded_Set_Iterator;
   begin
      Result.For_The_Container :=
        Address_Conversions.To_Pointer (For_The_Set'Address).all'Access;
      Reset (Result);
      return Result;
   end New_Iterator;

   --  Null containers

   Empty_Container : Set;
   pragma Warnings (Off, Empty_Container);

   function Null_Container return Unconstrained_Set is
   begin
      return Empty_Container;
   end Null_Container;

   --  Iterators

   procedure Reset (It : in out Bounded_Set_Iterator) is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      Tables.Reset (S.Rep, It.Bucket_Index, It.Index);
   end Reset;

   procedure Next (It : in out Bounded_Set_Iterator) is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      Tables.Next (S.Rep, It.Bucket_Index, It.Index);
   end Next;

   function Is_Done (It : Bounded_Set_Iterator) return Boolean is
      S : Unconstrained_Set'Class
     renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      return Tables.Is_Done (S.Rep, It.Bucket_Index, It.Index);
   end Is_Done;

   function Current_Item (It : Bounded_Set_Iterator) return Item is
      S : Unconstrained_Set'Class
     renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Item_Ptr (S.Rep, It.Bucket_Index, It.Index).all;
   end Current_Item;

   function Current_Item_Ptr (It : Bounded_Set_Iterator) return Item_Ptr is
      S : Unconstrained_Set'Class
     renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Item_Ptr (S.Rep, It.Bucket_Index, It.Index);
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Bounded_Set_Iterator) is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      Tables.Delete_Item_At (S.Rep, It.Bucket_Index, It.Index);
   end Delete_Item_At;

end BC.Containers.Sets.Bounded;
