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

package body BC.Containers.Stacks.Dynamic is

  function Create (Size : Positive) return Dyn_Stack is
    Temp : Dyn_Stack;
  begin
    Temp.Rep := Dyn_Stack_Nodes.Create (Size);
    return Temp;
  end Create;

  function "=" (Left, Right : Dyn_Stack) return Boolean is
    use Dyn_Stack_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Clear (Obj : in out Dyn_Stack) is
  begin
    Dyn_Stack_Nodes.Clear (Obj.Rep.all);
  end Clear;

  procedure Push (Obj : in out Dyn_Stack; Elem : Item) is
  begin
    Dyn_Stack_Nodes.Insert (Obj.Rep.all, Elem);
  end Push;

  procedure Pop (Obj : in out Dyn_Stack) is
  begin
    Dyn_Stack_Nodes.Remove (Obj.Rep.all, 1);
  end Pop;

  function Depth (Obj : in Dyn_Stack) return Natural is
  begin
    return Dyn_Stack_Nodes.Length (Obj.Rep.all);
  end Depth;

  function Is_Empty (Obj : Dyn_Stack) return Boolean is
  begin
    return Dyn_Stack_Nodes.Length (Obj.Rep.all) = 0;
  end Is_Empty;

  function Top (Obj : Dyn_Stack) return Item is
  begin
    return Dyn_Stack_Nodes.First (Obj.Rep.all);
  end Top;

  function Top (Obj : in Dyn_Stack) return Item_Ptr is
  begin
    return Dyn_Stack_Nodes.First (Obj.Rep.all'access);
  end Top;

  function Location (Obj : Dyn_Stack; Elem : Item) return Natural is
  begin
    return Dyn_Stack_Nodes.Location (Obj.Rep.all'access, Elem);
  end Location;

  procedure Preallocate (Obj : in out Dyn_Stack; Size : Natural) is
  begin
    Dyn_Stack_Nodes.Preallocate (Obj.Rep.all, Size);
  end Preallocate;

  procedure Set_Chunk_Size (Obj : in out Dyn_Stack; Size : Natural) is
  begin
    Dyn_Stack_Nodes.Set_Chunk_Size (Obj.Rep.all, Size);
  end Set_Chunk_Size;

  function Chunk_Size (Obj : Dyn_Stack) return Natural is
  begin
    return Dyn_Stack_Nodes.Chunk_Size (Obj.Rep.all);
  end Chunk_Size;

  procedure Purge (Obj : in out Dyn_Stack) is
  begin
    Dyn_Stack_Nodes.Clear (Obj.Rep.all);
  end Purge;

  procedure Add (Obj : in out Dyn_Stack; Elem : in out Item) is
  begin
    Dyn_Stack_Nodes.Append (Obj.Rep.all, Elem);
  end Add;

  function Cardinality (Obj : Dyn_Stack) return Integer is
  begin
    return Dyn_Stack_Nodes.Length (Obj.Rep.all);
  end Cardinality;

  function Item_At (Obj : in Dyn_Stack; Index : in Natural) return Item_Ptr is
  begin
    return Dyn_Stack_Nodes.Item_At (Obj.Rep, Index);
  end Item_At;

  procedure Initialize (Obj : in out Dyn_Stack) is
  begin
    Obj.Rep := Dyn_Stack_Nodes.Create;
  end Initialize;

  procedure Adjust (Obj : in out Dyn_Stack) is
  begin
    Obj.Rep := Dyn_Stack_Nodes.Create (Obj.Rep.all);
  end Adjust;

  procedure Finalize (Obj : in out Dyn_Stack) is
    use type Dyn_Stack_Nodes.Dyn_Node_Ref;
  begin
    if Obj.Rep /= null then
      Dyn_Stack_Nodes.Free (Obj.Rep);
    end if;
  end Finalize;

end BC.Containers.Stacks.Dynamic;
