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

   --  Private implementations

   --  XXX there is another Attach() which I don't understand

   procedure Attach (S : in out Unconstrained_Set; I : Item) is
   begin
      Tables.Bind (S.Rep, I, True);
   end Attach;

   procedure Detach (S : in out Unconstrained_Set; I : Item) is
   begin
      Tables.Unbind (S.Rep, I);
   end Detach;

   function Number_Of_Buckets (S : Unconstrained_Set) return Natural is
      pragma Warnings (Off, S);
   begin
      return Buckets;
   end Number_Of_Buckets;

   function Item_At (S : Unconstrained_Set;
                     Bucket, Index : Positive) return Item_Ptr is
      pragma Warnings (Off, Bucket);
   begin
      return Tables.Access_Item_At (S.Rep, Index);
   end Item_At;

   procedure Reset (It : in out Bounded_Set_Iterator) is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      It.Index := 0;
      if Extent (S) = 0 then
         It.Bucket_Index := 0;
      else
         It.Bucket_Index := 1;
         while It.Bucket_Index <= Number_Of_Buckets (S) loop
            if S.Rep.Buckets (It.Bucket_Index) > 0 then
               It.Index := S.Rep.Buckets (It.Bucket_Index);
               exit;
            end if;
            It.Bucket_Index := It.Bucket_Index + 1;
         end loop;
      end if;
   end Reset;

   procedure Next (It : in out Bounded_Set_Iterator) is
      S : Unconstrained_Set'Class
        renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      if It.Bucket_Index <= Number_Of_Buckets (S) then
         if S.Rep.Contents (It.Index).Next > 0 then
            It.Index := S.Rep.Contents (It.Index).Next;
         else
            It.Bucket_Index := It.Bucket_Index + 1;
            It.Index := 0;
            while It.Bucket_Index <= Number_Of_Buckets (S) loop
               if S.Rep.Buckets (It.Bucket_Index) > 0 then
                  It.Index := S.Rep.Buckets (It.Bucket_Index);
                  exit;
               end if;
               It.Bucket_Index := It.Bucket_Index + 1;
            end loop;
         end if;
      end if;
   end Next;

   function Is_Done (It : Bounded_Set_Iterator) return Boolean is
      S : Unconstrained_Set'Class
     renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      if It.Bucket_Index = 0
        or else It.Bucket_Index > Number_Of_Buckets (S) then
         return True;
      end if;
      if It.Index > 0 then
         return False;
      end if;
      declare
         package Conversions is new System.Address_To_Access_Conversions
           (Bounded_Set_Iterator'Class);
         P : Conversions.Object_Pointer := Conversions.To_Pointer (It'Address);
      begin
         P.Bucket_Index := P.Bucket_Index + 1;
         P.Index := 0;
         while P.Bucket_Index <= Number_Of_Buckets (S) loop
            if S.Rep.Buckets (P.Bucket_Index) > 0 then
               P.Index := S.Rep.Buckets (P.Bucket_Index);
               return False;
            end if;
            P.Bucket_Index := P.Bucket_Index + 1;
         end loop;
      end;
      return True;
   end Is_Done;

   function Current_Item (It : Bounded_Set_Iterator) return Item is
      S : Unconstrained_Set'Class
     renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return S.Rep.Contents (It.Index).Item;
   end Current_Item;

   function Current_Item_Ptr (It : Bounded_Set_Iterator) return Item_Ptr is
      --  XXX this should probably not be permitted!
      S : Unconstrained_Set'Class
     renames Unconstrained_Set'Class (It.For_The_Container.all);
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return Tables.Access_Item_At (S.Rep, It.Index);
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Bounded_Set_Iterator) is
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      raise BC.Not_Yet_Implemented;
   end Delete_Item_At;

   Empty_Container : Set;
   pragma Warnings (Off, Empty_Container);

   function Null_Container return Unconstrained_Set is
   begin
      return Empty_Container;
   end Null_Container;

end BC.Containers.Sets.Bounded;
