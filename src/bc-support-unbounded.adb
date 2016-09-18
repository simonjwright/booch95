--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2005, 2016 Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $Revision$
--  $Date$
--  $Author$

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

with BC.Support.Caching;

package body BC.Support.Unbounded is

   --  We can't take 'Access of components of constant (in parameter)
   --  objects; but we need to be able to do this so that we can
   --  update the cache (which doesn't violate the abstraction, just
   --  the Ada restriction). This technique is due to Matthew Heaney.
   package Allow_Access
   is new System.Address_To_Access_Conversions (Unb_Node);

   --  We can't take 'Access of non-aliased components. But if we
   --  alias discriminated objects they become constrained - even if
   --  the discriminant has a default.
   package Allow_Element_Access
   is new System.Address_To_Access_Conversions (Item);

   --  Support caching
   package Cache_Manager is new Caching.Cache_Manager
     (Container => Unb_Node,
      Node      => Node,
      Node_Ref  => Node_Ref);

   function Create (I : Item; Previous, Next : Node_Ref) return Node_Ref;
   pragma Inline (Create);

   function Create (I : Item; Previous, Next : Node_Ref) return Node_Ref is
      Result : Node_Ref;
   begin
      Result := new Node'(Element => I,
                          Previous => Previous,
                          Next => Next);
      if Previous /= null then
         Previous.Next := Result;
      end if;
      if Next /= null then
         Next.Previous := Result;
      end if;
      return Result;
   end Create;

   procedure Delete_Node is new
     Ada.Unchecked_Deallocation (Node, Node_Ref);

   procedure Update_Cache (Obj : in out Unb_Node; Index : Positive);

   procedure Update_Cache (Obj : in out Unb_Node; Index : Positive) is
      Cache : constant Caching.Cache_P := Cache_Manager.Get_Cache (Obj);
      Node : constant Node_Ref := Cache_Manager.Get_Node_Ref (Cache);
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Node /= null then
         if Index = Cache.Index then
            return;
         elsif Index = Cache.Index + 1 then
            Cache_Manager.Update (Cache, Node.Next, Index);
            return;
         elsif Index = Cache.Index - 1 then
            Cache_Manager.Update (Cache, Node.Previous, Index);
            return;
         end if;
      end if;
      declare
         Ptr : Node_Ref := Obj.Rep;
      begin
         for I in 1 .. Index - 1 loop
            Ptr := Ptr.Next;
         end loop;
         Cache_Manager.Update (Cache, Ptr, Index);
      end;
   end Update_Cache;

   function "=" (Left, Right : in Unb_Node) return Boolean is
   begin
      if Left.Size = Right.Size then
         declare
            Temp_L : Node_Ref := Left.Rep;
            Temp_R : Node_Ref := Right.Rep;
         begin
            while Temp_L /= null loop
               if Temp_L.Element /= Temp_R.Element then
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

   procedure Clear (Obj : in out Unb_Node) is
      Empty_Node : Unb_Node;
      Ptr : Node_Ref;
   begin
      while Obj.Rep /= null loop
         Ptr := Obj.Rep;
         Obj.Rep := Obj.Rep.Next;
         Delete_Node (Ptr);
      end loop;
      Obj := Empty_Node;
   end Clear;

   procedure Insert (Obj : in out Unb_Node; Elem : Item) is
      Cache : constant Caching.Cache_P := Cache_Manager.Get_Cache (Obj);
   begin
      Obj.Rep := Create (Elem, Previous => null, Next => Obj.Rep);
      if Obj.Last = null then
         Obj.Last := Obj.Rep;
      end if;
      Obj.Size := Obj.Size + 1;
      Cache_Manager.Update (Cache, Obj.Rep, 1);
   end Insert;

   procedure Insert (Obj : in out Unb_Node; Elem : Item; Before : Positive) is
   begin
      if Before > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Obj.Size = 0 or else Before = 1 then
         Insert (Obj, Elem);
      else
         declare
            Cache : Caching.Cache_P;
            Node : Node_Ref;
            Temp_Node : Node_Ref;
         begin
            Update_Cache (Obj, Before);
            Cache     := Cache_Manager.Get_Cache (Obj);
            Node      := Cache_Manager.Get_Node_Ref (Cache);
            Temp_Node := Create (Elem,
                                 Previous => Node.Previous,
                                 Next     => Node);
            if Temp_Node.Previous = null then
               Obj.Rep := Temp_Node;
            end if;
            Obj.Size := Obj.Size + 1;
            Cache_Manager.Update (Cache, Temp_Node, Cache.Index);
         end;
      end if;
   end Insert;

   procedure Append (Obj : in out Unb_Node; Elem : Item) is
      Cache : constant Caching.Cache_P := Cache_Manager.Get_Cache (Obj);
   begin
      Obj.Last := Create (Elem, Previous => Obj.Last, Next => null);
      if Obj.Last.Previous /= null then
         Obj.Last.Previous.Next := Obj.Last;
      end if;
      if Obj.Rep = null then
         Obj.Rep := Obj.Last;
      end if;
      Obj.Size := Obj.Size + 1;
      Cache_Manager.Update (Cache, Obj.Last, Obj.Size);
   end Append;

   procedure Append (Obj : in out Unb_Node; Elem : Item; After : Positive) is
   begin
      if After > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Obj.Size = 0 then
         Append (Obj, Elem);
      else
         declare
            Cache : Caching.Cache_P;
            Node : Node_Ref;
            Temp_Node : Node_Ref;
         begin
            Update_Cache (Obj, After);
            Cache := Cache_Manager.Get_Cache (Obj);
            Node := Cache_Manager.Get_Node_Ref (Cache);
            Temp_Node := Create (Elem,
                                 Previous => Node,
                                 Next => Node.Next);
            if Temp_Node.Previous /= null then
               Temp_Node.Previous.Next := Temp_Node;
            end if;
            if Temp_Node.Next = null then
               Obj.Last := Temp_Node;
            end if;
            Obj.Size := Obj.Size + 1;
            Cache_Manager.Update (Cache, Temp_Node, Cache.Index + 1);
         end;
      end if;
   end Append;

   procedure Remove (Obj : in out Unb_Node; From : Positive) is
   begin
      if From > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Obj.Size = 0 then
         raise BC.Underflow;
      end if;
      if Obj.Size = 1 then
         Clear (Obj);
      else
         declare
            Cache : Caching.Cache_P;
            Ptr : Node_Ref;
         begin
            Update_Cache (Obj, From);
            Cache := Cache_Manager.Get_Cache (Obj);
            Ptr := Cache_Manager.Get_Node_Ref (Cache);
            --  sorry about the .all'Access
            if Ptr.Previous = null then
               Obj.Rep := Ptr.Next;
            else
               Ptr.Previous.Next := Ptr.Next;
            end if;
            if Ptr.Next = null then
               Obj.Last := Ptr.Previous;
            else
               Ptr.Next.Previous := Ptr.Previous;
            end if;
            Obj.Size := Obj.Size - 1;
            if Ptr.Next /= null then
               Cache_Manager.Update (Cache, Ptr.Next, Cache.Index);
            elsif Ptr.Previous /= null then
               Cache_Manager.Update (Cache, Ptr.Previous, Cache.Index - 1);
            else
               Cache_Manager.Update (Cache, null, 0);
            end if;
            Delete_Node (Ptr);
         end;
      end if;
   end Remove;

   procedure Replace (Obj : in out Unb_Node; Index : Positive; Elem : Item) is
      Cache : constant Caching.Cache_P := Cache_Manager.Get_Cache (Obj);
      Node : Node_Ref := Cache_Manager.Get_Node_Ref (Cache);
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Node = null or else Cache.Index /= Index then
         Node := Obj.Rep;
            for I in 1 .. Obj.Size loop
               if I = Index then
                  Cache_Manager.Update (Cache, Node, Index);
                  exit;
               else
                  Node := Node.Next;
               end if;
            end loop;
      end if;
      Node.Element := Elem;
   end Replace;

   function Length (Obj : Unb_Node) return Natural is
   begin
      return Obj.Size;
   end Length;

   function First (Obj : Unb_Node) return Item is
   begin
      if Obj.Size = 0 then
         raise BC.Underflow;
      end if;
      return Obj.Rep.Element;
   end First;

   function Last (Obj : Unb_Node) return Item is
   begin
      if Obj.Size = 0 then
         raise BC.Underflow;
      end if;
      return Obj.Last.Element;
   end Last;

   function Item_At (Obj : Unb_Node; Index : Positive) return Item is
      Tmp : Item_Ptr;
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      Tmp := Item_At (Obj, Index);
      return Tmp.all;
   end Item_At;

   function Item_At (Obj : Unb_Node; Index : Positive) return Item_Ptr is
      U : constant Allow_Access.Object_Pointer
        := Allow_Access.To_Pointer (Obj'Address);
      --  Update_Cache takes Obj as in-out (it doesn't need to now?)
      --
      --  XXX not sure this will still be true? XXX
      --  Note, although (GNAT 3.11p) the value in Obj is successfully
      --  updated via U, the optimiser can get fooled; when we return
      --  next/previous cache hits, we must return via U. I don't
      --  think this is a bug; the pointer aliasing is a nasty trick,
      --  after all.
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      Update_Cache (U.all, Index);
      declare
         Cache : constant Caching.Cache_P
           := Cache_Manager.Get_Cache (Obj);
         Node : constant Node_Ref := Cache_Manager.Get_Node_Ref (Cache);
      begin
         return Item_Ptr
           (Allow_Element_Access.To_Pointer (Node.Element'Address));
      end;
   end Item_At;

   function Location (Obj : Unb_Node; Elem : Item; Start : Positive := 1)
                     return Natural is
      Cache : constant Caching.Cache_P := Cache_Manager.Get_Cache (Obj);
      Node : Node_Ref := Cache_Manager.Get_Node_Ref (Cache);
      --  U : constant Allow_Access.Object_Pointer
      --    := Allow_Access.To_Pointer (Obj'Address);
   begin
      --  XXX the C++ (which indexes from 0) nevertheless checks
      --  "start <= count". We have to special-case the empty Node;
      --  the C++ indexes from 0, so it can legally start with index 0
      --  when the Node is empty.
      if Obj.Size = 0 then
         return 0;
      end if;
      if Start > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Start = Cache.Index and then Elem = Node.Element then
         return Start;
      end if;
      Node := Obj.Rep;
      for I in 1 .. Start - 1 loop
         Node := Node.Next; -- advance to Start point
      end loop;
      for I in Start .. Obj.Size loop
         if Node.Element = Elem then
            Cache_Manager.Update (Cache, Node, I);
            return I;
         else
            Node := Node.Next;
         end if;
      end loop;
      return 0;
   end Location;

   procedure Adjust (U : in out Unb_Node) is
      Tmp : Node_Ref := U.Last;
   begin
      if Tmp /= null then
         U.Last := Create (Tmp.Element, Previous => null, Next => null);
         U.Rep := U.Last;
         Tmp := Tmp.Previous;  -- move to previous node from orig list
         while Tmp /= null loop
            U.Rep := Create (Tmp.Element,
                             Previous => null,
                             Next => U.Rep);
            Tmp := Tmp.Previous;
         end loop;
      end if;
      declare
         Cache : constant Caching.Cache_P := Cache_Manager.Get_Cache (U);
      begin
         Cache_Manager.Update (Cache, null, 0);
      end;
   end Adjust;

   procedure Finalize (U : in out Unb_Node) is
      Ptr : Node_Ref;
   begin
      --  code to delete Rep copied from Clear()
      while U.Rep /= null loop
         Ptr := U.Rep;
         U.Rep := U.Rep.Next;
         Delete_Node (Ptr);
      end loop;
   end Finalize;

   procedure Write_Unb_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : Unb_Node) is
      N : Node_Ref := Obj.Rep;
   begin
      Integer'Write (Stream, Obj.Size);
      while N /= null loop
         Item'Output (Stream, N.Element);
         N := N.Next;
      end loop;
   end Write_Unb_Node;

   procedure Read_Unb_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : out Unb_Node) is
      Count : Integer;
   begin
      Clear (Obj);
      Integer'Read (Stream, Count);
      for I in 1 .. Count loop
         declare
            Elem : constant Item := Item'Input (Stream);
         begin
            Append (Obj, Elem);
         end;
      end loop;
   end Read_Unb_Node;

end BC.Support.Unbounded;
