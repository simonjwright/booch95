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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with System.Address_To_Access_Conversions;

package body BC.Containers.Maps.Dynamic is

   procedure Clear (M : in out Map) is
   begin
      Tables.Clear (M.Rep);
   end Clear;

   procedure Bind
     (M : in out Map; K : Key; I : Item) is
   begin
      Tables.Bind (M.Rep, K, I);
   end Bind;

   procedure Rebind
     (M : in out Map; K : Key; I : Item) is
   begin
      Tables.Rebind (M.Rep, K, I);
   end Rebind;

   procedure Unbind (M : in out Map; K : Key) is
   begin
      Tables.Unbind (M.Rep, K);
   end Unbind;

   function Extent (M : Map) return Natural is
   begin
      return Tables.Extent (M.Rep);
   end Extent;

   function Is_Empty (M : Map) return Boolean is
   begin
      return Tables.Extent (M.Rep) = 0;
   end Is_Empty;

   function Is_Bound (M : Map; K : Key) return Boolean is
   begin
      return Tables.Is_Bound (M.Rep, K);
   end Is_Bound;

   function Item_Of (M : Map; K : Key) return Item is
   begin
      return Tables.Value_Of (M.Rep, K);
   end Item_Of;

   procedure Preallocate (M : in out Map; Size : Positive) is
   begin
      for B in 1 .. Buckets loop
         KC.Preallocate (M.Rep.Items (B), Size);
         IC.Preallocate (M.Rep.Values (B), Size);
      end loop;
   end Preallocate;

   procedure Set_Chunk_Size (M : in out Map; Size : Positive) is
   begin
      for B in 1 .. Buckets loop
         KC.Set_Chunk_Size (M.Rep.Items (B), Size);
         IC.Set_Chunk_Size (M.Rep.Values (B), Size);
      end loop;
   end Set_Chunk_Size;

   function Chunk_Size (M : Map) return Positive is
   begin
      return KC.Chunk_Size (M.Rep.Items (1));
   end Chunk_Size;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Map);

   function New_Iterator (For_The_Map : Map) return Iterator'Class is
      Result : Map_Iterator;
   begin
      Result.For_The_Container :=
        Address_Conversions.To_Pointer (For_The_Map'Address).all'Access;
      Reset (Result);
      return Result;
   end New_Iterator;

   --  Private implementations

   procedure Attach (M : in out Map; K : Key; I : Item) is
   begin
      Tables.Bind (M.Rep, K, I);
   end Attach;

   function Number_Of_Buckets (M : Map) return Natural is
   begin
      return Buckets;
   end Number_Of_Buckets;

   function Length (M : Map; Bucket : Positive) return Natural is
   begin
      return KC.Length (M.Rep.Items (Bucket));
   end Length;

   function Item_At
     (M : Map; Bucket, Index : Positive) return Item_Ptr is
   begin
      return IC.Item_At (M.Rep.Values (Bucket), Index);
   end Item_At;

   function Key_At
     (M : Map; Bucket, Index : Positive) return Key_Ptr is
   begin
      return KC.Item_At (M.Rep.Items (Bucket), Index);
   end Key_At;

   Empty_Container : Map;
   pragma Warnings (Off, Empty_Container);

   function Null_Container return Map is
   begin
      return Empty_Container;
   end Null_Container;

end BC.Containers.Maps.Dynamic;
