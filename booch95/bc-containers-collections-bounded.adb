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

package body BC.Containers.Collections.Bounded is

   function "=" (Left, Right : in Unconstrained_Collection) return Boolean is
      use Collection_Nodes;
   begin
      return Left.Rep = Right.Rep;
   end "=";

   procedure Clear (C : in out Unconstrained_Collection) is
   begin
      Collection_Nodes.Clear (C.Rep);
   end Clear;

   procedure Insert (C : in out Unconstrained_Collection; Elem : Item) is
   begin
      Collection_Nodes.Insert (C.Rep, Elem);
   end Insert;

   procedure Insert (C : in out Unconstrained_Collection;
                     Elem : Item;
                     Before : Positive) is
   begin
      Collection_Nodes.Insert (C.Rep, Elem, Before);
   end Insert;

   procedure Append (C : in out Unconstrained_Collection; Elem : Item) is
   begin
      Collection_Nodes.Append (C.Rep, Elem);
   end Append;

   procedure Append (C : in out Unconstrained_Collection;
                     Elem : Item;
                     After : Positive) is
   begin
      Collection_Nodes.Append (C.Rep, Elem, After);
   end Append;

   procedure Remove (C : in out Unconstrained_Collection;
                     At_Index : Positive) is
   begin
      Collection_Nodes.Remove (C.Rep, At_Index);
   end Remove;

   procedure Replace (C : in out Unconstrained_Collection;
                      At_Index : Positive;
                      Elem : Item) is
   begin
      Collection_Nodes.Replace (C.Rep, At_Index, Elem);
   end Replace;

   function Available (C : in Unconstrained_Collection) return Natural is
   begin
      return Collection_Nodes.Available (C.Rep);
   end Available;

   function Length (C : Unconstrained_Collection) return Natural is
   begin
      return Collection_Nodes.Length (C.Rep);
   end Length;

   function Is_Empty (C : Unconstrained_Collection) return Boolean is
   begin
      return Collection_Nodes.Length (C.Rep) = 0;
   end Is_Empty;

   function First (C : Unconstrained_Collection) return Item is
   begin
      return Collection_Nodes.First (C.Rep);
   end First;

   function Last (C : Unconstrained_Collection) return Item is
   begin
      return Collection_Nodes.Last (C.Rep);
   end Last;

   function Item_At
     (C : Unconstrained_Collection; At_Index : Positive) return Item is
   begin
      return Item_At (C, At_Index).all;
   end Item_At;

   function Location (C : Unconstrained_Collection;
                      Elem : Item) return Natural is
   begin
      return Collection_Nodes.Location (C.Rep, Elem);
   end Location;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Unconstrained_Collection);

   function New_Iterator
     (For_The_Collection : Unconstrained_Collection) return Iterator'Class is
      Result : Collection_Iterator;
   begin
      Result.For_The_Container :=
        Address_Conversions.To_Pointer (For_The_Collection'Address).all'Access;
      Reset (Result);
      return Result;
   end New_Iterator;

   function Item_At
     (C : Unconstrained_Collection; Index : Positive) return Item_Ptr is
   begin
      return Collection_Nodes.Item_At (C.Rep, Index);
   end Item_At;

   Empty_Container : Collection;
   pragma Warnings (Off, Empty_Container);

   function Null_Container return Unconstrained_Collection is
   begin
      return Empty_Container;
   end Null_Container;

end BC.Containers.Collections.Bounded;
