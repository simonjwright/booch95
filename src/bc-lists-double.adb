--  Copyright 1994 Grady Booch
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

package body BC.Lists.Double is

   --  We can't take 'Access of non-aliased components. But if we
   --  alias discriminated objects they become constrained - even if
   --  the discriminant has a default.
   package Allow_Element_Access
   is new System.Address_To_Access_Conversions (Item);

   function Create
     (I : Item; Previous, Next : Double_Node_Ref) return Double_Node_Ref;
   pragma Inline (Create);

   function Create
     (I : Item; Previous, Next : Double_Node_Ref) return Double_Node_Ref is
      Result : Double_Node_Ref;
   begin
      Result := new Double_Node'(Element => I,
                                 Previous => Previous,
                                 Next => Next,
                                 Count => 1);
      if Previous /= null then
         Previous.Next := Result;
      end if;
      if Next /= null then
         Next.Previous := Result;
      end if;
      return Result;
   end Create;

   procedure Delete is
      new Ada.Unchecked_Deallocation (Double_Node, Double_Node_Ref);

   function "=" (L, R : List) return Boolean is
   begin
      return L.Rep = R.Rep;
   end "=";

   procedure Clear (L : in out List) is
      Curr : Double_Node_Ref := L.Rep;
      Ptr  : Double_Node_Ref;
   begin
      while Curr /= null loop
         Ptr := Curr;
         Curr := Curr.Next;
         if Ptr.Count > 1 then
            Ptr.Count := Ptr.Count - 1;
            exit;
         else
            if Curr /= null then
               Curr.Previous := null;
            end if;
            Delete (Ptr);
         end if;
      end loop;
      L.Rep := null;
   end Clear;

   procedure Insert (L : in out List; Elem : Item) is
   begin
      --  Ensure we only insert at a list's head.
      if L.Rep /= null and then L.Rep.Previous /= null then
         raise BC.Not_Root;
      end if;
      L.Rep := Create (Elem, Previous => null, Next => L.Rep);
   end Insert;

   procedure Insert (L : in out List; From_List : in out List) is
      Ptr : Double_Node_Ref := From_List.Rep;
   begin
      --  Ensure we only insert at a list's head.
      if L.Rep /= null and then L.Rep.Previous /= null then
         raise BC.Not_Root;
      end if;
      if Ptr /= null then
         while Ptr.Next /= null loop
            Ptr := Ptr.Next;
         end loop;
         Ptr.Next := L.Rep;
         if L.Rep /= null then
            L.Rep.Previous := Ptr;
         end if;
         L.Rep := From_List.Rep;
         L.Rep.Count := L.Rep.Count + 1;
      end if;
   end Insert;

   procedure Insert (L : in out List; Elem : Item; Before : Positive) is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := L.Rep;
      Index : Positive := 1;
   begin
      if Curr = null or else Before = 1 then
         Insert (L, Elem);
      else
         while Curr /= null and then Index < Before loop
            Prev := Curr;
            Curr := Curr.Next;
            Index := Index + 1;
         end loop;
         if Curr = null then
            raise BC.Range_Error;
         end if;
         Prev.Next := Create (Elem, Previous => Prev, Next => Curr);
      end if;
   end Insert;

   procedure Insert (L : in out List;
                     From_List : in out List;
                     Before : Positive) is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := L.Rep;
      Ptr : Double_Node_Ref := From_List.Rep;
      Index : Positive := 1;
   begin
      if Ptr /= null then
         if Curr = null or else Before = 1 then
            Insert (L, From_List);
         else
            --  Ensure From_List is the head of a list.
            if Ptr.Previous /= null then
               raise BC.Not_Root;
            end if;
            while Curr /= null and then Index < Before loop
               Prev := Curr;
               Curr := Curr.Next;
               Index := Index + 1;
            end loop;
            if Curr = null then
               raise BC.Range_Error;
            end if;
            while Ptr.Next /= null loop
               Ptr := Ptr.Next;
            end loop;
            Ptr.Next := Curr;
            Curr.Previous := Ptr;
            Prev.Next := From_List.Rep;
            From_List.Rep.Previous := Prev;
            From_List.Rep.Count := From_List.Rep.Count + 1;
         end if;
      end if;
   end Insert;

   procedure Append (L : in out List; Elem : Item) is
      Curr : Double_Node_Ref := L.Rep;
   begin
      if Curr /= null then
         while Curr.Next /= null loop
            Curr := Curr.Next;
         end loop;
         Curr.Next := Create (Elem, Previous => Curr, Next => null);
      else
         L.Rep := Create (Elem, Previous => null, Next => null);
      end if;
   end Append;

   procedure Append (L : in out List; From_List : in out List) is
      Curr : Double_Node_Ref := L.Rep;
   begin
      --  Ensure From_List is the head of a list.
      if From_List.Rep /= null and then From_List.Rep.Previous /= null then
         raise BC.Not_Root;
      end if;
      if From_List.Rep /= null then
         if Curr /= null then
            while Curr.Next /= null loop
               Curr := Curr.Next;
            end loop;
         end if;
         if Curr /= null then
            Curr.Next := From_List.Rep;
            From_List.Rep.Previous := Curr;
         else
            L.Rep := From_List.Rep;
         end if;
         From_List.Rep.Count := From_List.Rep.Count + 1;
      end if;
   end Append;

   procedure Append (L : in out List; Elem : Item; After : Positive) is
      Curr : Double_Node_Ref := L.Rep;
      Index : Positive := 1;
   begin
      if Curr = null then
         Append (L, Elem);
      else
         while Curr /= null and then Index < After loop
            Curr := Curr.Next;
            Index := Index + 1;
         end loop;
         if Curr = null then
            raise BC.Range_Error;
         end if;
         Curr.Next := Create (Elem,
                                    Previous => Curr,
                                    Next => Curr.Next);
      end if;
   end Append;

   procedure Append (L : in out List;
                     From_List : in out List;
                     After : Positive) is
      Curr : Double_Node_Ref := L.Rep;
      Ptr : Double_Node_Ref := From_List.Rep;
      Index : Positive := 1;
   begin
      if Ptr /= null then
         if Curr = null then
            Append (L, From_List);
         else
            --  Ensure From_List is the head of a list.
            --  XXX check this logic!
            if From_List.Rep /= null and then
              From_List.Rep.Previous /= null
            then
               raise BC.Not_Root;
            end if;
            while Curr /= null and then Index < After loop
               Curr := Curr.Next;
               Index := Index + 1;
            end loop;
            if Curr = null then
               raise BC.Range_Error;
            end if;
            while Ptr.Next /= null loop
               Ptr := Ptr.Next;
            end loop;
            Ptr.Next := Curr.Next;
            if Curr.Next /= null then
               Curr.Next.Previous := Ptr;
            end if;
            Curr.Next := From_List.Rep;
            From_List.Rep.Previous := Curr;
            From_List.Rep.Count := From_List.Rep.Count + 1;
         end if;
      end if;
   end Append;

   procedure Remove (L : in out List; From : Positive) is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := L.Rep;
      Index : Positive := 1;
   begin
      while Curr /= null and then Index < From loop
         Prev := Curr;
         Curr := Curr.Next;
         Index := Index + 1;
      end loop;
      if Curr = null then
         raise BC.Range_Error;
      end if;
      --  Ensure we're not removing an aliased element.
      if Curr.Count /= 1 then
         raise BC.Referenced;
      end if;
      if Prev /= null then
         Prev.Next := Curr.Next;
      else
         L.Rep := Curr.Next;
      end if;
      if Curr.Next /= null then
         Curr.Next.Previous := Prev;
      end if;
      if Curr.Count > 1 then
         Curr.Count := Curr.Count - 1;
      else
         Delete (Curr);
      end if;
   end Remove;

   procedure Purge (L : in out List; From : Positive) is
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := L.Rep;
      Ptr : Double_Node_Ref;
      Index : Positive := 1;
   begin
      while Curr /= null and then Index < From loop
         Prev := Curr;
         Curr := Curr.Next;
         Index := Index + 1;
      end loop;
      if Curr = null then
         raise BC.Range_Error;
      end if;
      if Prev /= null then
         Prev.Next := null;
      else
         L.Rep := null;
      end if;
      while Curr /= null loop
         Curr.Previous := null;
         Ptr := Curr;
         Curr := Curr.Next;
         if Ptr.Count > 1 then
            Ptr.Count := Ptr.Count - 1;
            exit;
         else
            Delete (Ptr);
         end if;
      end loop;
   end Purge;

   procedure Purge (L : in out List; From : Positive; Count : Positive) is
      Prev, Ptr : Double_Node_Ref;
      Curr : Double_Node_Ref := L.Rep;
      Index : Positive := 1;
      Shared_Node_Found : Boolean := False;
   begin
      while Curr /= null and then Index < From loop
         Prev := Curr;
         Curr := Curr.Next;
         Index := Index + 1;
      end loop;
      if Curr = null then
         raise BC.Range_Error;
      end if;
      if Prev /= null then
         Prev.Next := null;
      else
         L.Rep := null;
      end if;
      Index := 1;
      while Curr /= null and then Index <= Count loop
         Ptr := Curr;
         Curr := Curr.Next;
         if not Shared_Node_Found then
            if Ptr.Count > 1 then
               Ptr.Count := Ptr.Count - 1;
               Shared_Node_Found := True;
            else
               if Curr /= null then
                  Curr.Previous := null;
                  Delete (Ptr);
               end if;
            end if;
         end if;
         Index := Index + 1;
      end loop;
      if Shared_Node_Found then
         Ptr.Next := null;
      end if;
      if Curr /= null then
         Curr.Previous := Prev;
         if Prev /= null then
            Prev.Next := Curr;
         else
            L.Rep := Curr;
         end if;
      end if;
   end Purge;

   procedure Preserve (L : in out List; From : Positive) is
      Temp : List;
   begin
      Share (Temp, L, From);
      Share_Head (L, Temp);
   end Preserve;

   procedure Preserve (L : in out List;
                       From : Positive;
                       Count : Positive) is
   begin
      Preserve (L, From);
      if Length (L) > Count then
         Purge (L, Count + 1); -- we start at 1, remember!
      end if;
   end Preserve;

   procedure Share (L : in out List;
                    With_List : List;
                    Starting_At : Positive) is
      Ptr : Double_Node_Ref := With_List.Rep;
      Index : Positive := 1;
   begin
      if Ptr = null then
         raise BC.Is_Null;
      end if;
      while Ptr /= null and then Index < Starting_At loop
         Ptr := Ptr.Next;
         Index := Index + 1;
      end loop;
      if Ptr = null then
         raise BC.Range_Error;
      end if;
      Clear (L);
      L.Rep := Ptr;
      L.Rep.Count := L.Rep.Count + 1;
   end Share;

   procedure Share_Head (L : in out List; With_List : in List) is
   begin
      if With_List.Rep = null then
         raise BC.Is_Null;
      end if;
      Clear (L);
      L.Rep := With_List.Rep;
      L.Rep.Count := L.Rep.Count + 1;
   end Share_Head;

   procedure Share_Foot (L : in out List; With_List : in List) is
      Ptr : Double_Node_Ref := With_List.Rep;
   begin
      if Ptr = null then
         raise BC.Is_Null;
      end if;
      Clear (L);
      while Ptr.Next /= null loop
         Ptr := Ptr.Next;
      end loop;
      L.Rep := Ptr;
      L.Rep.Count := L.Rep.Count + 1;
   end Share_Foot;

   procedure Swap_Tail (L : in out List; With_List : in out List) is
      Curr : Double_Node_Ref;
   begin
      if L.Rep = null then
         raise BC.Is_Null;
      end if;
      if With_List.Rep /= null and then With_List.Rep.Previous /= null then
         raise BC.Not_Root;
      end if;
      Curr := L.Rep.Next;
      L.Rep.Next := With_List.Rep;
      With_List.Rep.Previous := L.Rep;
      With_List.Rep := Curr;
      if With_List.Rep /= null then
         With_List.Rep.Previous := null;
      end if;
   end Swap_Tail;

   procedure Tail (L : in out List) is
      Curr : Double_Node_Ref := L.Rep;
   begin
      if L.Rep = null then
         raise BC.Is_Null;
      end if;
      L.Rep := L.Rep.Next;
      if L.Rep /= null then
         L.Rep.Count := L.Rep.Count + 1;
      end if;
      if Curr.Count > 1 then
         Curr.Count := Curr.Count - 1;
      else
         if L.Rep /= null then
            L.Rep.Count := L.Rep.Count - 1;
            L.Rep.Previous := null;
         end if;
         Delete (Curr);
      end if;
   end Tail;

   procedure Predecessor (L : in out List) is
   begin
      if L.Rep = null then
         raise BC.Is_Null;
      end if;
      if L.Rep.Previous = null then
         Clear (L);
      else
         L.Rep.Count := L.Rep.Count - 1;
         L.Rep := L.Rep.Previous;
         L.Rep.Count := L.Rep.Count + 1;
      end if;
   end Predecessor;

   procedure Set_Head (L : in out List; Elem : Item) is
   begin
      if L.Rep = null then
         raise BC.Is_Null;
      end if;
      L.Rep.Element := Elem;
   end Set_Head;

   procedure Set_Item (L : in out List; Elem : Item; At_Loc : Positive) is
      Curr : Double_Node_Ref := L.Rep;
      Index : Positive := 1;
   begin
      while Curr /= null and then Index < At_Loc loop
         Curr := Curr.Next;
         Index := Index + 1;
      end loop;
      if Curr = null then
         raise BC.Range_Error;
      end if;
      Curr.Element := Elem;
   end Set_Item;

   function Length (L : List) return Natural is
      Curr : Double_Node_Ref := L.Rep;
      Count : Natural := 0;
   begin
      while Curr /= null loop
         Curr := Curr.Next;
         Count := Count + 1;
      end loop;
      return Count;
   end Length;

   function Is_Null (L : List) return Boolean is
   begin
      return L.Rep = null;
   end Is_Null;

   function Is_Shared (L : List) return Boolean is
   begin
      if L.Rep /= null then
         return L.Rep.Count > 1;
      else
         return False;
      end if;
   end Is_Shared;

   function Is_Head (L : List) return Boolean is
   begin
      return L.Rep = null or else L.Rep.Previous = null;
   end Is_Head;

   function Head (L : List) return Item is
   begin
      if L.Rep = null then
         raise BC.Is_Null;
      end if;
      return L.Rep.Element;
   end Head;

   procedure Process_Head (L : in out List) is
   begin
      if L.Rep = null then
         raise BC.Is_Null;
      end if;
      Process (L.Rep.Element);
   end Process_Head;

   function Foot (L : List) return Item is
      Curr : Double_Node_Ref := L.Rep;
   begin
      if L.Rep = null then
         raise BC.Is_Null;
      end if;
      while Curr.Next /= null loop
         Curr := Curr.Next;
      end loop;
      return Curr.Element;
   end Foot;

   procedure Process_Foot (L : in out List) is
      Curr : Double_Node_Ref := L.Rep;
   begin
      if L.Rep = null then
         raise BC.Is_Null;
      end if;
      while Curr.Next /= null loop
         Curr := Curr.Next;
      end loop;
      Process (Curr.Element);
   end Process_Foot;

   function Item_At (L : List; Index : Positive) return Item is
   begin
      return Item_At (L, Index).all;
   end Item_At;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (List);

   function New_Iterator (For_The_List : List) return Iterator'Class is
      Result : List_Iterator;
   begin
      Result.For_The_List :=
        List_Base_Ptr (Address_Conversions.To_Pointer (For_The_List'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   function Item_At (L : List; Index : Positive) return Item_Ptr is
      Curr : Double_Node_Ref := L.Rep;
      Loc : Positive := 1;
   begin
      if L.Rep = null then
         raise BC.Is_Null;
      end if;
      while Curr /= null and then Loc < Index loop
         Curr := Curr.Next;
         Loc := Loc + 1;
      end loop;
      if Curr = null then
         raise BC.Range_Error;
      end if;
      if Curr = null then
         raise BC.Range_Error;
      end if;
      return Item_Ptr
        (Allow_Element_Access.To_Pointer (Curr.Element'Address));
   end Item_At;

   procedure Initialize (L : in out List) is
      pragma Warnings (Off, L);
   begin
      null;
   end Initialize;

   procedure Adjust (L : in out List) is
   begin
      if L.Rep /= null then
         L.Rep.Count := L.Rep.Count + 1;
      end if;
   end Adjust;

   procedure Finalize (L : in out List) is
   begin
      Clear (L);
   end Finalize;

   procedure Reset (It : in out List_Iterator) is
      L : List'Class renames List'Class (It.For_The_List.all);
   begin
      It.Index := L.Rep;
   end Reset;

   procedure Next (It : in out List_Iterator) is
   begin
      if It.Index /= null then
         It.Index := It.Index.Next;
      end if;
   end Next;

   function Is_Done (It : List_Iterator) return Boolean is
   begin
      return It.Index = null;
   end Is_Done;

   function Current_Item (It : List_Iterator) return Item is
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return It.Index.Element;
   end Current_Item;

   function Current_Item_Ptr (It : List_Iterator) return Item_Ptr is
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return Item_Ptr
        (Allow_Element_Access.To_Pointer (It.Index.Element'Address));
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out List_Iterator) is
      L : List'Class renames List'Class (It.For_The_List.all);
      Prev : Double_Node_Ref;
      Curr : Double_Node_Ref := L.Rep;
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      while Curr /= null and then Curr /= It.Index loop
         Prev := Curr;
         Curr := Curr.Next;
      end loop;
      if Curr = null then
         raise BC.Range_Error;
      end if;
      --  we need a writable version of the Iterator
      declare
         package Conversions is new System.Address_To_Access_Conversions
           (List_Iterator'Class);
         P : constant Conversions.Object_Pointer
           := Conversions.To_Pointer (It'Address);
      begin
         P.Index := Curr.Next;
      end;
      if Prev /= null then
         Prev.Next := Curr.Next;
      else
         L.Rep := Curr.Next;
      end if;
      if Curr.Next /= null then
         Curr.Next.Previous := Prev;
      end if;
      if Curr.Count > 1 then
         Curr.Count := Curr.Count - 1;
      else
         Delete (Curr);
      end if;
   end Delete_Item_At;

end BC.Lists.Double;
