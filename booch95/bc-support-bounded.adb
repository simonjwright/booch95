-- The Ada 95 Booch Components (Version 1.0 beta 1)
-- Copyright (C)1994-1997 Grady Booch and David Weller.  All Rights Reserved.
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
--  This file contains the definition for the static array-based class
--  used for the representation of bounded structures.

with Unchecked_Deallocation;
package body BC.Support.Bounded is

   function Create(Obj : in Bnd_Node) return Bnd_Node_Ref is
   begin
      return new Bnd_Node'(Elems=>Obj.Elems, Size=>Obj.Size);
   end Create;

   procedure Clear  (Obj : in out Bnd_Node) is
   begin
      Obj.Size := 0;
   end Clear;

   procedure Insert(Obj : in out Bnd_Node; Elem : Item) is
   begin
      pragma Assert(Obj.Size < Max_Size, "Attempt to insert into full array");
      Obj.Elems(2..Obj.Size + 1) := Obj.Elems(1..Obj.Size);
      Obj.Elems(1) := Elem;
      Obj.Size := Obj.Size + 1;
   end Insert;

   procedure Insert(Obj : in out Bnd_Node; Elem : Item; Before : Natural) is
   begin
      pragma Assert(Obj.Size < Max_Size, "Attempt to insert into full array");
      if (Obj.Size = 0) or else (Before = 1) then
         Insert(Obj,Elem);
      else
     Obj.Elems(Before + 1 .. Obj.Size + 1) := Obj.Elems(Before..Obj.Size);
     Obj.Elems(Before) := Elem;
     Obj.Size := Obj.Size + 1;
      end if;
   end Insert;

   procedure Append (Obj : in out Bnd_Node; Elem : Item) is
   begin
      pragma Assert(Obj.Size < Max_Size, "Attempt to insert into full array");
      Obj.Size := Obj.Size + 1;
      Obj.Elems(Obj.Size) := Elem;

   end Append;

   procedure Append (Obj : in out Bnd_Node; Elem : Item;
                          After : Natural) is
   begin
      pragma Assert(After <= Obj.Size, "Exceeded current bounded size");
      if ( After = Obj.Size ) then
	 Obj.Size := Obj.Size + 1;
	 Obj.Elems(Obj.Size) := Elem;
      else
	 Obj.Elems(After + 2 .. Obj.Size + 1) := Obj.Elems(After+1..Obj.Size);
	 Obj.Size := Obj.Size + 1;
	 Obj.Elems(After+1) := Elem;
      end if;
   end Append;

   procedure Remove (Obj : in out Bnd_Node; From : Natural) is
   begin
      pragma Assert( From <= Obj.Size );
      pragma Assert( Obj.Size > 0 );
      if Obj.Size = 1 then
         Clear(Obj);
      else
     Obj.Elems(From..Obj.Size-1) := Obj.Elems(From+1..Obj.Size);
     Obj.Size := Obj.Size - 1;
      end if;
   end Remove;

   procedure Replace(Obj : in out Bnd_Node; Index : Natural; Elem : Item) is
   begin
      pragma Assert( Index <= Obj.Size );
      Obj.Elems(Index) := Elem;
   end Replace;

   function Available(Obj: Bnd_Node) return Natural is
   begin
      return Max_Size - Obj.Size;
   end Available;

   function Length  (Obj : Bnd_Node) return Natural is
   begin
      return Obj.Size;
   end Length;

   function First   (Obj : Bnd_Node) return Item is
   begin
      pragma Assert( Obj.Size > 0 );
      return Obj.Elems(1);
   end First;

   function First   (Obj : access Bnd_Node) return Item_Ptr is
   begin
      return Obj.Elems(1)'Access;
   end First;

   function Last    (Obj : Bnd_Node) return Item is
   begin
      pragma Assert( Obj.Size > 0 );
      return Obj.Elems(Obj.Size);
   end Last;

   function Last    (Obj : access Bnd_Node) return Item_Ptr is
   begin
      pragma Assert( Obj.Size > 0 );
      return Obj.Elems(Obj.Size)'Access;
   end Last;

   function Item_At(Obj : Bnd_Node; Index : Natural) return Item is
   begin
      pragma Assert(Index > 0);
      return Obj.Elems(Index);
   end Item_At;

   function Item_At(Obj : access Bnd_Node; Index : Natural) return Item_Ptr is
   begin
      pragma Assert(Index > 0);
      return Obj.Elems(Index)'Access;
   end Item_At;

   function Location(Obj : access Bnd_Node; Elem : Item;
             Start : Natural := 1) return Natural is
   begin
      for I in Start..Obj.Size loop
         if Obj.Elems(I) = Elem then return I; end if;
      end loop;
      return 0;
   end Location;

   procedure Free(Obj : in out Bnd_Node_Ref) is
      procedure Delete_Node is new
	Unchecked_Deallocation(Object=>Bnd_Node,Name=>Bnd_Node_Ref);
   begin
      Delete_Node(Obj);
   end Free;
      
end BC.Support.Bounded;



