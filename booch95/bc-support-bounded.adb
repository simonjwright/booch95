--  Copyright (C) 1994-2001 Grady Booch, David Weller and Simon Wright.
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

package body BC.Support.Bounded is

   package BSE renames BC.Support.Exceptions;
   procedure Assert
   is new BSE.Assert ("BC.Support.Bounded");

   --  We can't take 'Access of components of constant (in parameter)
   --  objects; but we need to be able to do this so that we can
   --  return pointers to individual elements. This technique is due
   --  to Matthew Heaney.
   package Allow_Access
   is new System.Address_To_Access_Conversions (Elem_Array);

   --  We can't take 'Access of non-aliased components. But if we
   --  alias discriminated objects they become constrained - even if
   --  the discriminant has a default.
   package Allow_Element_Access
   is new System.Address_To_Access_Conversions (Item);

   function "=" (Left, Right : Bnd_Node) return Boolean is
   begin
      if Left.Size /= Right.Size then
         return False;
      else
         for I in 1 .. Left.Size loop
            if Item'(Item_At (Left, I)) /= Item'(Item_At (Right, I)) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end "=";

   procedure Clear (Obj : in out Bnd_Node) is
   begin
      Obj.Start := 0;
      Obj.Size := 0;
   end Clear;

   procedure Insert (Obj : in out Bnd_Node; Elem : Item) is
   begin
      Assert (Obj.Size < Max_Size,
              BC.Overflow'Identity,
              "Insert",
              BSE.Full);
      Obj.Start := (Obj.Start - 1) mod Max_Size;
      Obj.Size := Obj.Size + 1;
      Obj.Elems (Obj.Start) := Elem;
   end Insert;

   procedure Insert (Obj : in out Bnd_Node; Elem : Item; Before : Positive) is
   begin
      Assert (Before <= Obj.Size,
              BC.Range_Error'Identity,
              "Insert",
              BSE.Invalid_Index);
      Assert (Obj.Size < Max_Size,
              BC.Overflow'Identity,
              "Insert",
              BSE.Full);
      if Obj.Size = 0 or else Before = 1 then
         Insert (Obj, Elem);
      else
         --  We are inserting in the middle.
         --
         --  In the comments below, 'left' means the part of Elems
         --  before the element which the new entry is to be inserted
         --  before (indexed by Actual), 'right' means the part after.
         declare
            Elems : Elem_Array renames Obj.Elems;
            Start : Elem_Range renames Obj.Start;
            Actual : constant Elem_Range
              := (Obj.Start + Before - 1) mod Max_Size;
            Last : constant Elem_Range
              := (Obj.Start + Obj.Size - 1) mod Max_Size;
         begin
            if Start = 0 or else Start > Actual then
               --  the left part is wedged, shift the right part up
               Elems (Actual + 1 .. Last + 1) := Elems (Actual .. Last);
               Elems (Actual) := Elem;
            elsif Last = Elem_Range'Last or else Last < Actual then
               --  the right part is wedged, shift the left part down
               Elems (Start - 1 .. Actual - 2) := Elems (Start .. Actual - 1);
               Start := Start - 1;
               Elems (Actual - 1) := Elem;
            elsif Before < Obj.Size / 2 then
               --  the left part is shorter, shift it down
               Elems (Start - 1 .. Actual - 2) := Elems (Start .. Actual - 1);
               Start := Start - 1;
               Elems (Actual - 1) := Elem;
            else
               --  the right part is shorter, shift it up
               Elems (Actual + 1 .. Last + 1) := Elems (Actual .. Last);
               Elems (Actual) := Elem;
            end if;
            Obj.Size := Obj.Size + 1;
         end;
      end if;
   end Insert;

   procedure Append (Obj : in out Bnd_Node; Elem : Item) is
   begin
      Assert (Obj.Size < Max_Size,
              BC.Overflow'Identity,
              "Append",
              BSE.Full);
      Obj.Size := Obj.Size + 1;
      Obj.Elems ((Obj.Start + Obj.Size - 1) mod Max_Size) := Elem;
   end Append;

   procedure Append (Obj : in out Bnd_Node; Elem : Item; After : Positive) is
   begin
      Assert (After <= Obj.Size,
              BC.Range_Error'Identity,
              "Append",
              BSE.Invalid_Index);
      Assert (Obj.Size < Max_Size,
              BC.Overflow'Identity,
              "Append",
              BSE.Full);
      if Obj.Size = 0 or else After = Obj.Size then
         Append (Obj, Elem);
      else
         Insert (Obj, Elem, Before => After + 1);
      end if;
   end Append;

   procedure Remove (Obj : in out Bnd_Node; From : Positive) is
   begin
      Assert (From <= Obj.Size,
              BC.Range_Error'Identity,
              "Remove",
              BSE.Invalid_Index);
      --  XXX can this ever happen, given the test above?
      Assert (Obj.Size > 0,
              BC.Underflow'Identity,
              "Remove",
              BSE.Empty);
      if Obj.Size = 1 then
         Clear (Obj);
      elsif From = 1 then
         Obj.Start := (Obj.Start + 1) mod Max_Size;
         Obj.Size := Obj.Size - 1;
      elsif From = Obj.Size then
         Obj.Size := Obj.Size - 1;
      else
         --  We are removing from the middle.
         --
         --  In the comments below, 'left' means the part of Elems
         --  before the element to be removed (indexed by Actual),
         --  'right' means the part after.
         declare
            Elems : Elem_Array renames Obj.Elems;
            Start : Elem_Range renames Obj.Start;
            Actual : constant Elem_Range
              := (Obj.Start + From - 1) mod Max_Size;
            Last : constant Elem_Range
              := (Obj.Start + Obj.Size - 1) mod Max_Size;
         begin
            if Start > Actual then
               --  the left part wraps round; shift the right part down
               Elems (Actual .. Last - 1) := Elems (Actual + 1 .. Last);
            elsif Actual > Last then
               --  the right part wraps round; shift the left part up
               Elems (Start + 1 .. Actual) := Elems (Start .. Actual - 1);
               Start := Start + 1;
            elsif From < Obj.Size / 2 then
               --  the left part is shorter
               Elems (Start + 1 .. Actual) := Elems (Start .. Actual - 1);
               Start := Start + 1;
            else
               --  the right part is shorter
               Elems (Actual .. Last - 1) := Elems (Actual + 1 .. Last);
            end if;
            Obj.Size := Obj.Size - 1;
         end;
      end if;
   end Remove;

   procedure Replace (Obj : in out Bnd_Node; Index : Positive; Elem : Item) is
   begin
      Assert (Index <= Obj.Size,
              BC.Range_Error'Identity,
              "Replace",
              BSE.Invalid_Index);
      Obj.Elems ((Obj.Start + Index - 1) mod Max_Size) := Elem;
   end Replace;

   function Available (Obj : Bnd_Node) return Natural is
   begin
      return Max_Size - Obj.Size;
   end Available;

   function Length (Obj : Bnd_Node) return Natural is
   begin
      return Obj.Size;
   end Length;

   function First (Obj : Bnd_Node) return Item is
   begin
      Assert (Obj.Size > 0,
              BC.Underflow'Identity,
              "First",
              BSE.Empty);
      return Obj.Elems (Obj.Start);
   end First;

   function First (Obj : Bnd_Node) return Item_Ptr is
      E : Allow_Access.Object_Pointer
        := Allow_Access.To_Pointer (Obj.Elems'Address);
   begin
      Assert (Obj.Size > 0,
              BC.Underflow'Identity,
              "First",
              BSE.Empty);
      return Item_Ptr
        (Allow_Element_Access.To_Pointer (E (Obj.Start)'Address));
   end First;

   function Last (Obj : Bnd_Node) return Item is
   begin
      Assert (Obj.Size > 0,
              BC.Underflow'Identity,
              "Last",
              BSE.Empty);
      return Obj.Elems ((Obj.Start + Obj.Size - 1) mod Max_Size);
   end Last;

   function Last (Obj : Bnd_Node) return Item_Ptr is
      E : Allow_Access.Object_Pointer
        := Allow_Access.To_Pointer (Obj.Elems'Address);
   begin
      Assert (Obj.Size > 0,
              BC.Underflow'Identity,
              "Last",
              BSE.Empty);
      return Item_Ptr
        (Allow_Element_Access.To_Pointer
         (E ((Obj.Start + Obj.Size - 1) mod Max_Size)'Address));
   end Last;

   function Item_At (Obj : Bnd_Node; Index : Positive) return Item is
   begin
      Assert (Index in 1 .. Obj.Size,
              BC.Range_Error'Identity,
              "Item_At",
              BSE.Invalid_Index);
      return Obj.Elems ((Obj.Start + Index - 1) mod Max_Size);
   end Item_At;

   function Item_At (Obj : Bnd_Node; Index : Positive) return Item_Ptr is
      E : Allow_Access.Object_Pointer
        := Allow_Access.To_Pointer (Obj.Elems'Address);
   begin
      Assert (Index in 1 .. Obj.Size,
              BC.Range_Error'Identity,
              "Item_At",
              BSE.Invalid_Index);
      return Item_Ptr
        (Allow_Element_Access.To_Pointer
         (E ((Obj.Start + Index - 1) mod Max_Size)'Address));
   end Item_At;

   function Location (Obj : Bnd_Node;
                      Elem : Item;
                      Start : Positive := 1) return Natural is
   begin
      if Obj.Size = 0 then
         return 0;
      end if;
      Assert (Start <= Obj.Size,
              BC.Range_Error'Identity,
              "Start",
              BSE.Invalid_Index);
      for I in Start .. Obj.Size loop
         if Obj.Elems ((Obj.Start + I - 1) mod Max_Size) = Elem then
            return I;
         end if;
      end loop;
      return 0;
   end Location;

end BC.Support.Bounded;
