-- Copyright (C) 1994-2000 Grady Booch, David Weller and Simon Wright.
-- All Rights Reserved.
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

-- $Id$

with Ada.Unchecked_Deallocation;
with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Support.Dynamic is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Support.Dynamic");

  -- We can't take 'Access of non-aliased components. But if we alias
  -- discriminated objects they become constrained - even if the
  -- discriminant has a default.
  package Allow_Element_Access
  is new System.Address_To_Access_Conversions (Item);

  procedure Delete_Arr is
     new Ada.Unchecked_Deallocation (Dyn_Arr, Dyn_Arr_Ref);
  procedure Delete_Node is
     new Ada.Unchecked_Deallocation (Dyn_Node, Dyn_Node_Ref);

  function Create (From : Dyn_Node) return Dyn_Node_Ref is
    New_Node: Dyn_Node_Ref :=
        new Dyn_Node'(Ada.Finalization.Controlled with
                      Ref => new Dyn_Arr (1 .. From.Size + From.Chunk_Size),
                      Size => From.Size,
                      Chunk_Size => From.Chunk_Size);
  begin
    New_Node.Ref (1 .. New_Node.Size) := From.Ref (1 .. From.Size);
    return New_Node;
  end Create;

  function Create (Size : Positive := 10) return Dyn_Node_Ref is
  begin
    return new Dyn_Node'(Ada.Finalization.Controlled with
                         Ref => new Dyn_Arr (1 .. Size),
                         Size => 0,
                         Chunk_Size => Size);
  end Create;

  function "=" (Left, Right : Dyn_Node) return Boolean is
  begin
    if Left.Size /= Right.Size then
      return False;
    else
      return Left.Ref (1 .. Left.Size) = Right.Ref (1 .. Left.Size);
    end if;
  end "=";

  procedure Clear (Obj : in out Dyn_Node) is
  begin
    Delete_Arr (Obj.Ref);
    Preallocate (Obj);
    Obj.Size := 0;
  end Clear;

  procedure Extend (Obj : in out Dyn_Node) is
    Temp : Dyn_Arr_Ref;
  begin
    Temp := new Dyn_Arr (1 .. Obj.Ref'Last + Obj.Chunk_Size);
    Temp (1 .. Obj.Size) := Obj.Ref (1 .. Obj.Size);
    Delete_Arr (Obj.Ref);
    Obj.Ref := Temp;
  end Extend;

  procedure Insert (Obj : in out Dyn_Node; Elem : Item) is
  begin
    if Obj.Size = Obj.Ref'Last then
      Extend (Obj);
    end if;
    Obj.Ref (2 .. Obj.Size + 1) := Obj.Ref (1 .. Obj.Size);
    Obj.Ref (1) := Elem;
    Obj.Size := Obj.Size + 1;
  end Insert;

  procedure Insert (Obj : in out Dyn_Node; Elem : Item; Before : Positive) is
  begin
    Assert (Before <= Obj.Size,
            BC.Range_Error'Identity,
            "Insert",
            BSE.Invalid_Index);
    if Obj.Size = 0 or else Before = 1 then
      Insert (Obj, Elem);
    else
      if Obj.Size = Obj.Ref'Last then
        Extend (Obj);
      end if;
      Obj.Ref (Before + 1 .. Obj.Size + 1) := Obj.Ref (Before .. Obj.Size);
      Obj.Ref (Before) := Elem;
      Obj.Size := Obj.Size + 1;
    end if;
  end Insert;

  procedure Append (Obj : in out Dyn_Node; Elem : Item) is
  begin
    if Obj.Size >= Obj.Ref'Last then
      Extend (Obj);
    end if;
    Obj.Size := Obj.Size + 1;
    Obj.Ref (Obj.Size) := Elem;
  end Append;

  procedure Append (Obj : in out Dyn_Node; Elem : Item; After : Positive) is
  begin
    Assert (After <= Obj.Size,
            BC.Range_Error'Identity,
            "Append",
            BSE.Invalid_Index);
    if Obj.Size = Obj.Ref'Last then
      Extend (Obj);
    end if;
    if After = Obj.Size then
      Obj.Size := Obj.Size + 1;
      Obj.Ref (Obj.Size) := Elem;
    else
      Obj.Ref (After + 2 .. Obj.Size + 1) := Obj.Ref (After+1 .. Obj.Size);
      Obj.Size := Obj.Size + 1;
      Obj.Ref (After + 1) := Elem;
    end if;
  end Append;

  procedure Remove (Obj : in out Dyn_Node; From : Positive) is
  begin
    Assert (From <= Obj.Size,
            BC.Range_Error'Identity,
            "Remove",
            BSE.Invalid_Index);
    Assert (Obj.Size > 0,
            BC.Underflow'Identity,
            "Remove",
            BSE.Empty);
    if Obj.Size = 1 then
      Clear (Obj);
    else
      Obj.Ref (From .. Obj.Size - 1) := Obj.Ref (From + 1 .. Obj.Size);
      Obj.Size := Obj.Size - 1;
    end if;
  end Remove;

  procedure Replace (Obj : in out Dyn_Node; Index : Positive; Elem : Item) is
  begin
    Assert (Index <= Obj.Size,
            BC.Range_Error'Identity,
            "Replace",
            BSE.Invalid_Index);
    Obj.Ref (Index) := Elem;
  end Replace;

  function Length (Obj : Dyn_Node) return Natural is
  begin
    return Obj.Size;
  end Length;

  function First (Obj : Dyn_Node) return Item is
  begin
    Assert (Obj.Size > 0,
            BC.Underflow'Identity,
            "First",
            BSE.Empty);
    return Obj.Ref (1);
  end First;

  function First (Obj : Dyn_Node) return Item_Ptr is
  begin
    Assert (Obj.Size > 0,
            BC.Underflow'Identity,
            "First",
            BSE.Empty);
    return Item_Ptr
       (Allow_Element_Access.To_Pointer (Obj.Ref (1)'Address));
  end First;

  function Last (Obj : Dyn_Node) return Item is
  begin
    Assert (Obj.Size > 0,
            BC.Underflow'Identity,
            "Last",
            BSE.Empty);
    return Obj.Ref (Obj.Size);
  end Last;

  function Last (Obj : Dyn_Node) return Item_Ptr is
  begin
    Assert (Obj.Size > 0,
            BC.Underflow'Identity,
            "Last",
            BSE.Empty);
    return Item_Ptr
       (Allow_Element_Access.To_Pointer (Obj.Ref (Obj.Size)'Address));
  end Last;

  function Item_At (Obj : Dyn_Node; Index : Positive) return Item is
  begin
    Assert (Index <= Obj.Size,
            BC.Range_Error'Identity,
            "Item_At",
            BSE.Invalid_Index);
    return Obj.Ref (Index);
  end Item_At;

  function Item_At (Obj : Dyn_Node; Index : Positive) return Item_Ptr is
  begin
    Assert (Index <= Obj.Size,
            BC.Range_Error'Identity,
            "Item_At",
            BSE.Invalid_Index);
    return Item_Ptr
       (Allow_Element_Access.To_Pointer (Obj.Ref (Index)'Address));
  end Item_At;

  function Location (Obj : Dyn_Node; Elem : Item; Start : Positive := 1)
                     return Natural is
  begin
    -- XXX the C++ (which indexes from 0) nevertheless checks "start <= count"
    -- We have to special-case the empty Node; the C++ indexes from 0, so
    -- it can legally start with index 0 when the Node is empty.
    if Obj.Size = 0 then
      return 0;
    end if;
    Assert (Start <= Obj.Size,
            BC.Range_Error'Identity,
            "Location",
            BSE.Invalid_Index);
    for I in Start .. Obj.Size loop
      if Obj.Ref (I) = Elem then
        return I;
      end if;
    end loop;
    return 0;  -- Not located
  end Location;

  procedure Preallocate (Obj : in out Dyn_Node; New_Length : Natural := 10) is
    Temp : Dyn_Arr_Ref;
    Last : Natural;
  begin
    -- XXX I don't think this algorithm is very clever! we really shouldn't
    -- have to allocate a temporary and then delete it ..
    if Obj.Ref /= null then
      Temp := new Dyn_Arr (1 .. Obj.Ref'Last);
      Temp (1 .. Obj.Ref'Last) := Obj.Ref.all;
      Last := Obj.Ref'Last;
      Delete_Arr (Obj.Ref);
    else
      Last := 0;
    end if;
    Obj.Ref := new Dyn_Arr (1 .. Last + New_Length);
    if Last /= 0 then -- something was in the array already
      Obj.Ref (1 .. Obj.Size) := Temp (1 .. Obj.Size);
      Delete_Arr (Temp);
    end if;
  end Preallocate;

  procedure Set_Chunk_Size (Obj: in out Dyn_Node; Size : Natural) is
  begin
    Obj.Chunk_Size := Size;
  end Set_Chunk_Size;

  function Chunk_Size (Obj : in Dyn_Node) return Natural is
  begin
    return Obj.Chunk_Size;
  end Chunk_Size;

  procedure Free (Obj : in out Dyn_Node_Ref) is
  begin
    Delete_Node (Obj);
  end Free;

  procedure Initialize (D : in out Dyn_Node) is
  begin
    D.Ref := new Dyn_Arr (1 .. 10);
    D.Size := 0;
    D.Chunk_Size := 10;
  end Initialize;

  procedure Adjust (D : in out Dyn_Node) is
    Tmp : Dyn_Arr_Ref := new Dyn_Arr (1 .. D.Ref'Last);
  begin
    Tmp (1 .. D.Size) := D.Ref (1 .. D.Size);
    D.Ref := Tmp;
  end Adjust;

  procedure Finalize (D : in out Dyn_Node) is
  begin
    if D.Ref /= null then
      Delete_Arr (D.Ref);
      D.Ref := null;
    end if;
  end Finalize;

end BC.Support.Dynamic;
