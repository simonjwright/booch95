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
--  This file contains the definition for the list-based class
--  used for the representation of unbounded structures.

with Ada.Unchecked_Deallocation;
package body BC.Support.Unbounded is

   use type Nodes.Node_Ref;

   procedure Free is new
     Ada.Unchecked_Deallocation(Nodes.Node, Nodes.Node_Ref);

   procedure Reattach(Obj : in Nodes.Node_Ref) is
   begin
      if Obj.Prev /= null then
	 Obj.Prev.Next := Obj;
      end if;
      if Obj.Next /= null then
	 Obj.Next.Prev := Obj;
      end if;
   end Reattach;

   function Create(From : Unb_Node) return Unb_Node is
      Obj : Unb_Node := From;
      Ptr : Nodes.Node_Ref := From.Last;
   begin
      if Ptr /= null then
         Obj.Last := new Nodes.Node'(Ptr.Elem,null,null);
	 Reattach(Obj.Last);
         Obj.Rep := Obj.Last;
         Ptr := Ptr.Prev;
         while Ptr /= null loop
            Obj.Rep := new Nodes.Node'(Ptr.Elem,null, Obj.Rep);
	    Reattach(Obj.Rep);
            Ptr := Ptr.Prev;
         end loop;
      end if;
      Obj.Size := From.Size;
      return Obj;
   end Create;

   function "="(Left, Right : in Unb_Node) return Boolean is
   begin
      if Left.Size = Right.Size then
         declare
            Temp_L : Nodes.Node_Ref := Left.Rep;
            Temp_R : Nodes.Node_Ref := Right.Rep;
         begin
            while Temp_L /= null loop
               if Temp_L.Elem /= Temp_R.Elem then
                  return False;
               end if;
               Temp_L := Temp_L.Next;
               Temp_R := Temp_R.Next;
            end loop;
            return True;
         end;
      else
         return False;
      end if;
   end "=";

   procedure Clear  (Obj : in out Unb_Node) is
      Empty_Node : Unb_Node;
      Ptr : Nodes.Node_Ref;
   begin
      while Obj.Rep /= null loop
         Ptr := Obj.Rep;
         Obj.Rep := Obj.Rep.Next;
         Free(Ptr);
      end loop;
      Obj := Empty_Node;
   end Clear;

   procedure Insert(Obj : in out Unb_Node; Elem : Item) is
   begin
      Obj.Rep := new Nodes.Node'(Elem, null, Obj.Rep);
      Reattach(Obj.Rep);
      if Obj.Last = null then
	 Obj.Last := Obj.Rep;
      end if;
      Obj.Size := Obj.Size + 1;
      Obj.Cache := Obj.Rep;
      Obj.Cache_Index := 1;
   end Insert;

   procedure Insert(Obj : in out Unb_Node; Elem : Item; Before : Natural) is
   begin
      pragma Assert(Before <= Obj.Size);
      if (Obj.Size = 0) or else (Before <= 1) then
         Insert(Obj,Elem);
      else
         declare
	    AObj      : aliased Unb_Node := Obj;
            Temp_Item : Item := Item_At(AObj'access, Before);  -- loads cache
            Temp_Node : Nodes.Node_Ref;
         begin
            Temp_Node := new Nodes.Node'(Elem, Obj.Cache.Prev, Obj.Cache);
	    Reattach(Temp_Node);
	    if Temp_Node.Prev = null then
	       Obj.Rep := Temp_Node;
	    end if;
	    Obj.Size := Obj.Size + 1;
	    Obj.Cache := Temp_Node;
	 end;
      end if;
   end Insert;

   procedure Append (Obj : in out Unb_Node; Elem : Item) is
   begin
      Obj.Last := new Nodes.Node'(Elem, Obj.Last, null);
      Reattach(Obj.Last);
      if Obj.Rep = null then
         Obj.Rep := Obj.Last;
      end if;
      Obj.Size := Obj.Size + 1;
      Obj.Cache := Obj.Last;
      Obj.Cache_Index := Obj.Size;
   end Append;

   procedure Append (Obj : in out Unb_Node; Elem : Item; After : Natural) is
   begin
      pragma Assert(After <= Obj.Size);
      if (Obj.Size = 0) or else (After <= 1) then
         Append(Obj,Elem);
      else
         declare
	    AObj      : aliased Unb_Node := Obj;
            Temp_Item : Item := Item_At(AObj'access, After);  -- loads cache
            Temp_Node : Nodes.Node_Ref;
        begin
            Temp_Node := new Nodes.Node'(Elem, Obj.Cache, Obj.Cache.Next);
	    Reattach(Temp_Node);
            if Temp_Node.Next = null then
               Obj.Last := Temp_Node;
            end if;
            Obj.Size := Obj.Size + 1;
            Obj.Cache := Temp_Node;
            Obj.Cache_Index := Obj.Cache_Index + 1;
	end;
      end if;
   end Append;

   procedure Remove (Obj : in out Unb_Node; From : Natural) is
   begin
      pragma Assert( From <= Obj.Size );
      pragma Assert( Obj.Size > 0 );
      if Obj.Size = 1 then
         Clear(Obj);
      else
         declare
	    AObj      : aliased Unb_Node := Obj;
            Temp_Item : Item := Item_At(AObj'access, From);
            Ptr : Nodes.Node_Ref := AObj.Cache;
         begin
	    Obj.Cache := AObj.Cache; -- Xfer Cache value back to Obj
            if Ptr.Prev = null then
               Obj.Rep := Ptr.Next;
            else
               Ptr.Prev.Next := Ptr.Next;
            end if;
            if Ptr.Next = null then
               Obj.Last := Ptr.Prev;
            else
               Ptr.Next.Prev := Ptr.Prev;
            end if;
            Obj.Size := Obj.Size - 1;
            if Ptr.Next /= null then
               Obj.Cache := Ptr.Next;
            elsif Ptr.Prev /= null then
               Obj.Cache := Ptr.Prev;
               Obj.Cache_Index := Obj.Cache_Index - 1;
            else
               Obj.Cache := null;
               Obj.Cache_Index := 0;  
            end if;
            Free(Ptr);
         end;
      end if;
   end Remove;

   procedure Replace(Obj : in out Unb_Node; Index : Positive; Elem : Item) is
   begin
      pragma Assert( Index <= Obj.Size );
      if not ( (obj.Cache /= null) and then (Index = Obj.Cache_Index)) then
         declare
            Ptr : Nodes.Node_Ref := Obj.Rep;
         begin
            for i in 1..Obj.Size loop
               if i = Index then
                  Obj.Cache := Ptr;
                  Obj.Cache_Index := i;
                  exit;
               else
                  Ptr := Ptr.Next;
               end if;
            end loop;
         end;
      end if;
      Obj.Cache.Elem := Elem;
   end Replace;

   function Length  (Obj : Unb_Node) return Natural is
   begin
      return Obj.Size;
   end Length;

   function First   (Obj : Unb_Node) return Item is
   begin
      pragma Assert( Obj.Size > 0 );
      return Obj.Rep.Elem;
   end First;

   function First   (Obj : access Unb_Node) return Item_Ptr is
   begin
      pragma Assert( Obj.Size > 0 );
      return Obj.Rep.Elem'access;
   end First;

   function Last    (Obj : Unb_Node) return Item is
   begin
      pragma Assert( Obj.Size > 0 );
      return Obj.Last.Elem;
   end ;

   function Last    (Obj : access Unb_Node) return Item_Ptr is
   begin
      pragma Assert( Obj.Size > 0 );
      return  Obj.Last.Elem'access;
   end ;

   function Item_At(Obj : access Unb_Node; Index : Positive) return Item is
      Tmp : Item_Ptr;
   begin
      Tmp := Item_At(Obj,Index);
      return Tmp.all;
   end Item_At;

   function Item_At(Obj : access Unb_Node; Index : Positive) return Item_Ptr is
         Ptr : Nodes.Node_Ref := Obj.Rep;
   begin
      if Obj.Cache /= null then
         if Index = Obj.Cache_Index then
            return Obj.Cache.Elem'access;
	 end if;
      elsif Index = Obj.Cache_Index + 1 then
	 Obj.Cache := Obj.Cache.Next;
	 Obj.Cache_Index := Obj.Cache_Index + 1;
	 return Obj.Cache.Elem'access;
      elsif Index = Obj.Cache_Index - 1 then
	 Obj.Cache := Obj.Cache.Prev;
	 Obj.Cache_Index := Obj.Cache_Index - 1;
	 return Obj.Cache.Elem'access;
      end if;
      for i in 1..Obj.Size loop
	 if i = Index then
	    Obj.Cache := Ptr;
	    Obj.Cache_Index := i;
	    return Ptr.Elem'access;
	 else
	    Ptr := Ptr.Next;
	 end if;
      end loop;
      return Ptr.Elem'access;
   end Item_At;

   function Location(Obj : access Unb_Node; Elem : Item; Start : Positive := 1) 
		     return Natural is
      Ptr : Nodes.Node_Ref := Obj.Rep;
   begin
      pragma Assert( Start < Obj.Size );
      if (Start = Obj.Cache_Index) and then (Elem = Obj.Cache.Elem) then
         return Obj.Cache_Index;
      end if;
      for i in 1..Start-1 loop
         Ptr := Ptr.Next; -- advance to Start point
      end loop;
      for i in Start..Obj.Size loop
         if Ptr.Elem = Elem then
            Obj.Cache := Ptr;
            Obj.Cache_Index := i;
            return i;
         else
            Ptr := Ptr.Next;
         end if;
      end loop;
      return 0;
   end Location;

end BC.Support.Unbounded;


