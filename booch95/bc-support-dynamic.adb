-- Copyright (C) 1994-1998 Grady Booch, David Weller and Simon Wright.
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

package body BC.Support.Dynamic is

  procedure Delete_Arr is
     new Ada.Unchecked_Deallocation (Dyn_Arr, Dyn_Arr_Ref);
  procedure Delete_Node is
     new Ada.Unchecked_Deallocation (Dyn_Node, Dyn_Node_Ref);

  function Create (From : Dyn_Node) return Dyn_Node_Ref is
    Arr : Dyn_Arr_Ref := new Dyn_Arr (1 .. From.Size + From.Chunk_Size);
    New_Node: Dyn_Node_Ref := new Dyn_Node;
  begin
    Arr (1..From.Size) := From.Ref (1..From.Size);
    New_Node.all := From;
    New_Node.Ref := Arr;
    return New_Node;
  end Create;

  function Create (Size : Positive := 10) return Dyn_Node_Ref is
    Arr : Dyn_Arr_Ref := new Dyn_Arr (1..Size);
  begin
    return new Dyn_Node'(Ref => Arr, Size => 0, Chunk_Size => Size);
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
    pragma Assert (Before <= Obj.Size);
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
    pragma Assert (After <= Obj.Size);
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
    pragma Assert (From <= Obj.Size);
    if Obj.Size = 1 then
      Clear (Obj);
    else
      Obj.Ref (From .. Obj.Size-1) := Obj.Ref (From + 1 .. Obj.Size);
      Obj.Size := Obj.Size - 1;
    end if;
  end Remove;

  procedure Replace (Obj : in out Dyn_Node; Index : Positive; Elem : Item) is
  begin
    pragma Assert (Index <= Obj.Size);
    Obj.Ref (Index) := Elem;
  end Replace;

  function Length (Obj : Dyn_Node) return Natural is
  begin
    return Obj.Size;
  end Length;

  function First (Obj : Dyn_Node) return Item is
  begin
    return Obj.Ref (1);
  end First;

  function First (Obj : access Dyn_Node) return Item_Ptr is
  begin
    return Obj.Ref (1)'access;
  end First;

  function Last (Obj : Dyn_Node) return Item is
  begin
    pragma Assert (Obj.Size > 0);
    return Obj.Ref (Obj.Size);
  end Last;

  function Last (Obj : access Dyn_Node) return Item_Ptr is
  begin
    pragma Assert (Obj.Size > 0);
    return Obj.Ref (Obj.Size)'access;
  end Last;

  function Item_At (Obj : access Dyn_Node; Index : Positive) return Item is
  begin
    pragma Assert (Index <= Obj.Size);
    return Obj.Ref (Index);
  end Item_At;

  function Item_At (Obj : access Dyn_Node; Index : Positive) return Item_Ptr is
  begin
    pragma Assert (Index <= Obj.Size);
    return Obj.Ref (Index)'access;
  end Item_At;

  function Location (Obj : access Dyn_Node; Elem : Item; Start : Positive := 1)
                     return Natural is
  begin
    pragma Assert (Start <= Obj.Size);
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
    if Obj.Ref /= null then
      Delete_Arr (Obj.Ref);
    end if;
    Delete_Node (Obj);
  end Free;

end BC.Support.Dynamic;
