--  Copyright 1994 Grady Booch
--  Copyright 2005 Martin Krischik
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2014 Simon Wright <simon@pushface.org>

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

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

package body BC.Support.Indefinite_Unbounded is

   --  We can't take 'Access of components of constant (in parameter)
   --  objects; but we need to be able to do this so that we can
   --  update the cache (which doesn't violate the abstraction, just
   --  the Ada restriction). This technique is due to Matthew Heaney.
   package Allow_Access
   is new System.Address_To_Access_Conversions (Unb_Node);

   --  We can't take 'Access of non-aliased components. But if we
   --  alias discriminated objects they become constrained - even if
   --  the discriminant has a default.

   function Create (I : Item; Previous, Next : Node_Ref) return Node_Ref;
   pragma Inline (Create);

   function Create (I : Item; Previous, Next : Node_Ref) return Node_Ref is
      Result : Node_Ref;
   begin
      Result := new Node'(Element => Smart.Create (Value => I),
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
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Obj.Cache /= null then
         if Index = Obj.Cache_Index then
            return;
         elsif Index = Obj.Cache_Index + 1 then
            Obj.Cache := Obj.Cache.Next;
            Obj.Cache_Index := Index;
            return;
         elsif Index = Obj.Cache_Index - 1 then
            Obj.Cache := Obj.Cache.Previous;
            Obj.Cache_Index := Index;
            return;
         end if;
      end if;
      declare
         Ptr : Node_Ref := Obj.Rep;
      begin
         for I in 1 .. Index - 1 loop
            Ptr := Ptr.Next;
         end loop;
         Obj.Cache := Ptr;
         Obj.Cache_Index := Index;
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
               if Smart.Value (Temp_L.Element)
                 /= Smart.Value (Temp_R.Element)
               then
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
      Ptr : Node_Ref;
   begin
      while Obj.Rep /= null loop
         Ptr := Obj.Rep;
         Obj.Rep := Obj.Rep.Next;
         Delete_Node (Ptr);
      end loop;
      Obj.Last := null;
      Obj.Size := 0;
      Obj.Cache := null;
      Obj.Cache_Index := 0;
   end Clear;

   procedure Insert (Obj : in out Unb_Node; Elem : Item) is
   begin
      Obj.Rep := Create (Elem, Previous => null, Next => Obj.Rep);
      if Obj.Last = null then
         Obj.Last := Obj.Rep;
      end if;
      Obj.Size := Obj.Size + 1;
      Obj.Cache := Obj.Rep;
      Obj.Cache_Index := 1;
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
            Temp_Node : Node_Ref;
         begin
            Update_Cache (Obj, Before);
            Temp_Node := Create (Elem,
                                 Previous => Obj.Cache.Previous,
                                 Next => Obj.Cache);
            if Temp_Node.Previous = null then
               Obj.Rep := Temp_Node;
            end if;
            Obj.Size := Obj.Size + 1;
            Obj.Cache := Temp_Node;
         end;
      end if;
   end Insert;

   procedure Append (Obj : in out Unb_Node; Elem : Item) is
   begin
      Obj.Last := Create (Elem, Previous => Obj.Last, Next => null);
      if Obj.Last.Previous /= null then
         Obj.Last.Previous.Next := Obj.Last;
      end if;
      if Obj.Rep = null then
         Obj.Rep := Obj.Last;
      end if;
      Obj.Size := Obj.Size + 1;
      Obj.Cache := Obj.Last;
      Obj.Cache_Index := Obj.Size;
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
            Temp_Node : Node_Ref;
         begin
            Update_Cache (Obj, After);
            Temp_Node := Create (Elem,
                                       Previous => Obj.Cache,
                                       Next => Obj.Cache.Next);
            if Temp_Node.Previous /= null then
               Temp_Node.Previous.Next := Temp_Node;
            end if;
            if Temp_Node.Next = null then
               Obj.Last := Temp_Node;
            end if;
            Obj.Size := Obj.Size + 1;
            Obj.Cache := Temp_Node;
            Obj.Cache_Index := Obj.Cache_Index + 1;
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
            Ptr : Node_Ref;
         begin
            Update_Cache (Obj, From);
            Ptr := Obj.Cache;
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
               Obj.Cache := Ptr.Next;
            elsif Ptr.Previous /= null then
               Obj.Cache := Ptr.Previous;
               Obj.Cache_Index := Obj.Cache_Index - 1;
            else
               Obj.Cache := null;
               Obj.Cache_Index := 0;
            end if;
            Delete_Node (Ptr);
         end;
      end if;
   end Remove;

   procedure Replace (Obj : in out Unb_Node; Index : Positive; Elem : Item) is
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      if not ((Obj.Cache /= null) and then (Index = Obj.Cache_Index)) then
         declare
            Ptr : Node_Ref := Obj.Rep;
         begin
            for I in 1 .. Obj.Size loop
               if I = Index then
                  Obj.Cache := Ptr;
                  Obj.Cache_Index := I;
                  exit;
               else
                  Ptr := Ptr.Next;
               end if;
            end loop;
         end;
      end if;
      Obj.Cache.Element := Smart.Create (Value => Elem);
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
      return Smart.Value (Obj.Rep.Element);
   end First;

   function Last (Obj : Unb_Node) return Item is
   begin
      if Obj.Size = 0 then
         raise BC.Underflow;
      end if;
      return Smart.Value (Obj.Last.Element);
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
      return Smart.Value_Access (U.Cache.Element);
   end Item_At;

   function Location (Obj : Unb_Node; Elem : Item; Start : Positive := 1)
                     return Natural is
      Ptr : Node_Ref := Obj.Rep;
      U : constant Allow_Access.Object_Pointer
        := Allow_Access.To_Pointer (Obj'Address);
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
      if Start = Obj.Cache_Index
        and then Elem = Smart.Value (Obj.Cache.Element)
      then
         return Obj.Cache_Index;
      end if;
      for I in 1 .. Start - 1 loop
         Ptr := Ptr.Next; -- advance to Start point
      end loop;
      for I in Start .. Obj.Size loop
         if Smart.Value (Ptr.Element) = Elem then
            U.Cache := Ptr;
            U.Cache_Index := I;
            return I;
         else
            Ptr := Ptr.Next;
         end if;
      end loop;
      return 0;
   end Location;

   procedure Adjust (U : in out Unb_Node) is
      Tmp : Node_Ref := U.Last;
   begin
      if Tmp /= null then
         U.Last := Create (Smart.Value (Tmp.Element),
                           Previous => null,
                           Next => null);
         U.Rep := U.Last;
         Tmp := Tmp.Previous;  -- move to previous node from orig list
         while Tmp /= null loop
            U.Rep := Create (Smart.Value (Tmp.Element),
                                   Previous => null,
                                   Next => U.Rep);
            Tmp := Tmp.Previous;
         end loop;
      end if;
      U.Cache := null;
      U.Cache_Index := 0;
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
         Item'Output (Stream, Smart.Value (N.Element));
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

end BC.Support.Indefinite_Unbounded;
