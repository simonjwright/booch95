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
--  This file contains the implementation of the bounded queue.

package body BC.Containers.Queues.Bounded is

   use Bnd_Queue_Nodes;

   procedure Clear  (Obj : in out Bnd_Queue) is
   begin
      Bnd_Queue_Nodes.Clear(Obj.Rep.all);
   end Clear;

   procedure Append (Obj : in out Bnd_Queue; Elem : Item) is
   begin
      Bnd_Queue_Nodes.Append(Obj.Rep.all, Elem);
   end Append;

   procedure Pop    (Obj : in out Bnd_Queue) is
   begin
      Bnd_Queue_Nodes.Remove(Obj.Rep.all, 1);
   end Pop;

   procedure Remove (Obj : in out Bnd_Queue; From : Natural) is
   begin
      Bnd_Queue_Nodes.Remove(Obj.Rep.all, From);
   end Remove;

   function Available (Obj: in Bnd_Queue) return Natural is
   begin
      return Bnd_Queue_Nodes.Available(Obj.Rep.all);
   end Available;

   function Length  (Obj : Bnd_Queue) return Natural is
   begin
      return Bnd_Queue_Nodes.Length(Obj.Rep.all);
   end Length;

   function Is_Empty(Obj : Bnd_Queue) return Boolean is
   begin
      return Bnd_Queue_Nodes.Length(Obj.Rep.all) = 0;
   end Is_Empty;

   function Front   (Obj : Bnd_Queue) return Item is
   begin
      return Bnd_Queue_Nodes.First(Obj.Rep.all);
   end Front;

   function Front   (Obj : in Bnd_Queue) return Item_Ptr is
   begin
      return Bnd_Queue_Nodes.First(Obj.Rep.all'access);
   end Front;

   function Location(Obj : in Bnd_Queue; Elem : Item) return Natural is
   begin
      return Bnd_Queue_Nodes.Location(Obj.Rep, Elem);
   end Location;

   function "="(Left, Right : Bnd_Queue) return Boolean is
   begin
      return Left.Rep.all = Right.Rep.all;
   end "=";

   function Cardinality(Obj : Bnd_Queue) return Integer is
   begin
      return Bnd_Queue_Nodes.Length(Obj.Rep.all);
   end Cardinality;

   procedure Purge(Obj : in out Bnd_Queue) is
   begin
      Bnd_Queue_Nodes.Clear(Obj.Rep.all);
   end Purge;

   procedure Add(Obj : in out Bnd_Queue; Elem : in out Item) is
   begin
      Bnd_Queue_Nodes.Append(Obj.Rep.all, Elem);
   end Add;

   function Item_At(Obj : in Bnd_Queue; Index : in Natural) return Item_Ptr is
   begin
      return Bnd_Queue_Nodes.Item_At(Obj.Rep, Index);
   end Item_at;

   procedure Adjust(Obj : in out Bnd_Queue) is
   begin
      Obj.Rep := Bnd_Queue_Nodes.Create(Obj.Rep.all);
   end Adjust;

   procedure Finalize(Obj : in out Bnd_Queue) is
   begin
      Free(Obj.Rep);
   end Finalize;

end BC.Containers.Queues.Bounded;

