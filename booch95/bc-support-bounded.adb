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

package body BC.Support.Bounded is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Support.Bounded");

  -- We can't take 'Access of components of constant (in parameter)
  -- objects; but we need to be able to do this so that we can return
  -- pointers to individual elements. This technique is due to Matthew
  -- Heaney.
  package Allow_Access
  is new System.Address_To_Access_Conversions (Elem_Array);

  -- We can't take 'Access of non-aliased components. But if we alias
  -- discriminated objects they become constrained - even if the
  -- discriminant has a default.
  package Allow_Element_Access
  is new System.Address_To_Access_Conversions (Item);

  function Create (From : in Bnd_Node) return Bnd_Node_Ref is
  begin
    return new Bnd_Node'(Elems => From.Elems, Size => From.Size);
  end Create;

  function "=" (Left, Right : Bnd_Node) return Boolean is
  begin
    if Left.Size /= Right.Size then
      return False;
    else
      return Left.Elems (1 .. Left.Size) = Right.Elems (1 .. Left.Size);
    end if;
  end "=";

  procedure Clear (Obj : in out Bnd_Node) is
  begin
    Obj.Size := 0;
  end Clear;

  procedure Insert (Obj : in out Bnd_Node; Elem : Item) is
  begin
    Assert (Obj.Size < Max_Size,
            BC.Overflow'Identity,
            "Insert",
            BSE.Full);
    Obj.Elems (2 .. Obj.Size + 1) := Obj.Elems (1 .. Obj.Size);
    Obj.Elems (1) := Elem;
    Obj.Size := Obj.Size + 1;
  end Insert;

  procedure Insert (Obj : in out Bnd_Node; Elem : Item; Before : Positive) is
  begin
    Assert (Obj.Size < Max_Size,
            BC.Overflow'Identity,
            "Insert",
            BSE.Full);
    if Obj.Size = 0 or else Before = 1 then
      Insert (Obj, Elem);
    else
      Obj.Elems (Before + 1 .. Obj.Size + 1) := Obj.Elems (Before .. Obj.Size);
      Obj.Elems (Before) := Elem;
      Obj.Size := Obj.Size + 1;
    end if;
  end Insert;

  procedure Append (Obj : in out Bnd_Node; Elem : Item) is
  begin
    Assert (Obj.Size < Max_Size,
            BC.Overflow'Identity,
            "Append",
            BSE.Full);
    Obj.Size := Obj.Size + 1;
    Obj.Elems (Obj.Size) := Elem;
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
    if After = Obj.Size then
      Obj.Size := Obj.Size + 1;
      Obj.Elems (Obj.Size) := Elem;
    else
      Obj.Elems (After + 2 .. Obj.Size + 1) := Obj.Elems (After + 1 .. Obj.Size);
      Obj.Size := Obj.Size + 1;
      Obj.Elems (After + 1) := Elem;
    end if;
  end Append;

  procedure Remove (Obj : in out Bnd_Node; From : Positive) is
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
      Obj.Elems (From .. Obj.Size - 1) := Obj.Elems (From + 1 .. Obj.Size);
      Obj.Size := Obj.Size - 1;
    end if;
  end Remove;

  procedure Replace (Obj : in out Bnd_Node; Index : Positive; Elem : Item) is
  begin
    Assert (Index <= Obj.Size,
            BC.Range_Error'Identity,
            "Replace",
            BSE.Invalid_Index);
    Obj.Elems (Index) := Elem;
  end Replace;

  function Available (Obj: Bnd_Node) return Natural is
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
    return Obj.Elems (1);
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
       (Allow_Element_Access.To_Pointer (E (1)'Address));
  end First;

  function Last (Obj : Bnd_Node) return Item is
  begin
    Assert (Obj.Size > 0,
            BC.Underflow'Identity,
            "Last",
            BSE.Empty);
    return Obj.Elems(Obj.Size);
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
       (Allow_Element_Access.To_Pointer (E (Obj.Size)'Address));
  end Last;

  function Item_At (Obj : Bnd_Node; Index : Positive) return Item is
  begin
    Assert (Index in 1 .. Obj.Size,
            BC.Range_Error'Identity,
            "Item_At",
            BSE.Invalid_Index);
    return Obj.Elems (Index);
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
       (Allow_Element_Access.To_Pointer (E (Index)'Address));
  end Item_At;

  function Location (Obj : Bnd_Node;
                     Elem : Item;
                     Start : Natural := 1) return Natural is
  begin
    -- XXX the C++ (which indexes from 0) nevertheless checks "start <= count"
    -- We have to special-case the empty Node; the C++ indexes from 0, so
    -- it can legally start with index 0 when the Node is empty.
    if Obj.Size = 0 then
      return 0;
    end if;
    Assert (Start <= Obj.Size,
            BC.Range_Error'Identity,
            "Start",
            BSE.Invalid_Index);
    for I in Start .. Obj.Size loop
      if Obj.Elems (I) = Elem then
        return I;
      end if;
    end loop;
    return 0;
  end Location;

  procedure Free (Obj : in out Bnd_Node_Ref) is
    procedure Delete_Node is
       new Ada.Unchecked_Deallocation (Bnd_Node, Bnd_Node_Ref);
  begin
    Delete_Node (Obj);
  end Free;

end BC.Support.Bounded;
