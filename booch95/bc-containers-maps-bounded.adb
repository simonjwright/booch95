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

with System.Address_To_Access_Conversions;

package body BC.Containers.Maps.Bounded is

   function "=" (L, R : Unconstrained_Map) return Boolean is
   begin
      return Tables."=" (L.Rep, R.Rep);
   end "=";

   procedure Clear (M : in out Unconstrained_Map) is
   begin
      Tables.Clear (M.Rep);
   end Clear;

   procedure Bind
     (M : in out Unconstrained_Map; K : Key; I : Item) is
   begin
      Tables.Bind (M.Rep, K, I);
   end Bind;

   procedure Rebind
     (M : in out Unconstrained_Map; K : Key; I : Item) is
   begin
      Tables.Rebind (M.Rep, K, I);
   end Rebind;

   procedure Unbind (M : in out Unconstrained_Map; K : Key) is
   begin
      Tables.Unbind (M.Rep, K);
   end Unbind;

   function Available (M : Unconstrained_Map) return Natural is
   begin
      return M.Maximum_Size - M.Rep.Size;
   end Available;

   function Extent (M : Unconstrained_Map) return Natural is
   begin
      return Tables.Extent (M.Rep);
   end Extent;

   function Is_Empty (M : Unconstrained_Map) return Boolean is
   begin
      return Tables.Extent (M.Rep) = 0;
   end Is_Empty;

   function Is_Bound (M : Unconstrained_Map; K : Key) return Boolean is
   begin
      return Tables.Is_Bound (M.Rep, K);
   end Is_Bound;

   function Item_Of (M : Unconstrained_Map; K : Key) return Item is
   begin
      return Tables.Value_Of (M.Rep, K);
   end Item_Of;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Unconstrained_Map);

   function New_Iterator
     (For_The_Map : Unconstrained_Map) return Iterator'Class is
      Result : Bounded_Map_Iterator;
   begin
      Result.For_The_Container :=
        Address_Conversions.To_Pointer (For_The_Map'Address).all'Access;
      Reset (Result);
      return Result;
   end New_Iterator;

   --  Private implementations

   procedure Attach (M : in out Unconstrained_Map; K : Key; I : Item) is
   begin
      Tables.Bind (M.Rep, K, I);
   end Attach;

   function Number_Of_Buckets (M : Unconstrained_Map) return Natural is
      pragma Warnings (Off, M);
   begin
      return M.Number_Of_Buckets;
   end Number_Of_Buckets;

   function Length (M : Unconstrained_Map; Bucket : Positive) return Natural is
   begin
      return Tables.Bucket_Extent (M.Rep, Bucket);
   end Length;

   function Item_At
     (M : Unconstrained_Map; Bucket, Index : Positive) return Item_Ptr is
      pragma Warnings (Off, Bucket);
   begin
      return Tables.Access_Value_At (M.Rep, Index);
   end Item_At;

   function Key_At
     (M : Unconstrained_Map; Bucket, Index : Positive) return Key_Ptr is
      pragma Warnings (Off, Bucket);
   begin
      return Tables.Access_Item_At (M.Rep, Index);
   end Key_At;

   procedure Reset (It : in out Bounded_Map_Iterator) is
      M : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      It.Index := 0;
      if Extent (M) = 0 then
         It.Bucket_Index := 0;
      else
         It.Bucket_Index := 1;
         while It.Bucket_Index <= Number_Of_Buckets (M) loop
            if M.Rep.Buckets (It.Bucket_Index) > 0 then
               It.Index := M.Rep.Buckets (It.Bucket_Index);
               exit;
            end if;
            It.Bucket_Index := It.Bucket_Index + 1;
         end loop;
      end if;
   end Reset;

   procedure Next (It : in out Bounded_Map_Iterator) is
      M : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      if It.Bucket_Index <= Number_Of_Buckets (M) then
         if M.Rep.Contents (It.Index).Next > 0 then
            It.Index := M.Rep.Contents (It.Index).Next;
         else
            It.Bucket_Index := It.Bucket_Index + 1;
            It.Index := 0;
            while It.Bucket_Index <= Number_Of_Buckets (M) loop
               if M.Rep.Buckets (It.Bucket_Index) > 0 then
                  It.Index := M.Rep.Buckets (It.Bucket_Index);
                  exit;
               end if;
               It.Bucket_Index := It.Bucket_Index + 1;
            end loop;
         end if;
      end if;
   end Next;

   function Is_Done (It : Bounded_Map_Iterator) return Boolean is
      M : Unconstrained_Map'Class
     renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      if It.Bucket_Index = 0
        or else It.Bucket_Index > Number_Of_Buckets (M) then
         return True;
      end if;
      if It.Index > 0 then
         return False;
      end if;
      declare
         package Conversions is new System.Address_To_Access_Conversions
           (Bounded_Map_Iterator'Class);
         P : Conversions.Object_Pointer := Conversions.To_Pointer (It'Address);
      begin
         P.Bucket_Index := P.Bucket_Index + 1;
         P.Index := 0;
         while P.Bucket_Index <= Number_Of_Buckets (M) loop
            if M.Rep.Buckets (P.Bucket_Index) > 0 then
               P.Index := M.Rep.Buckets (P.Bucket_Index);
               return False;
            end if;
            P.Bucket_Index := P.Bucket_Index + 1;
         end loop;
      end;
      return True;
   end Is_Done;

   function Current_Item (It : Bounded_Map_Iterator) return Item is
      M : Unconstrained_Map'Class
     renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return M.Rep.Contents (It.Index).Value;
   end Current_Item;

   function Current_Item (It : Bounded_Map_Iterator) return Item_Ptr is
      --  XXX this should probably not be permitted!
      M : Unconstrained_Map'Class
     renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return Tables.Access_Value_At (M.Rep, It.Index);
   end Current_Item;

   procedure Delete_Item_At (It : in out Bounded_Map_Iterator) is
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      raise BC.Not_Yet_Implemented;
   end Delete_Item_At;

   Empty_Container : Map;
   pragma Warnings (Off, Empty_Container);

   function Null_Container return Unconstrained_Map is
   begin
      return Empty_Container;
   end Null_Container;

end BC.Containers.Maps.Bounded;
