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

with System.Address_To_Access_Conversions;

package body BC.Containers.Stacks.Dynamic is

  function Create (Size : Positive) return Dynamic_Stack is
    Temp : Dynamic_Stack;
  begin
    Temp.Rep := Dynamic_Stack_Nodes.Create (Size);
    return Temp;
  end Create;

  function "=" (Left, Right : Dynamic_Stack) return Boolean is
    use Dynamic_Stack_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Clear (Obj : in out Dynamic_Stack) is
  begin
    Dynamic_Stack_Nodes.Clear (Obj.Rep.all);
  end Clear;

  procedure Push (Obj : in out Dynamic_Stack; Elem : Item) is
  begin
    Dynamic_Stack_Nodes.Insert (Obj.Rep.all, Elem);
  end Push;

  procedure Pop (Obj : in out Dynamic_Stack) is
  begin
    Dynamic_Stack_Nodes.Remove (Obj.Rep.all, 1);
  end Pop;

  function Depth (Obj : in Dynamic_Stack) return Natural is
  begin
    return Dynamic_Stack_Nodes.Length (Obj.Rep.all);
  end Depth;

  function Is_Empty (Obj : Dynamic_Stack) return Boolean is
  begin
    return Dynamic_Stack_Nodes.Length (Obj.Rep.all) = 0;
  end Is_Empty;

  function Top (Obj : Dynamic_Stack) return Item is
  begin
    return Dynamic_Stack_Nodes.First (Obj.Rep.all);
  end Top;

  function Top (Obj : in Dynamic_Stack) return Item_Ptr is
  begin
    return Dynamic_Stack_Nodes.First (Obj.Rep.all);
  end Top;

  function Location (Obj : Dynamic_Stack; Elem : Item) return Natural is
  begin
    return Dynamic_Stack_Nodes.Location (Obj.Rep.all, Elem);
  end Location;

  procedure Preallocate (Obj : in out Dynamic_Stack; Size : Natural) is
  begin
    Dynamic_Stack_Nodes.Preallocate (Obj.Rep.all, Size);
  end Preallocate;

  procedure Set_Chunk_Size (Obj : in out Dynamic_Stack; Size : Natural) is
  begin
    Dynamic_Stack_Nodes.Set_Chunk_Size (Obj.Rep.all, Size);
  end Set_Chunk_Size;

  function Chunk_Size (Obj : Dynamic_Stack) return Natural is
  begin
    return Dynamic_Stack_Nodes.Chunk_Size (Obj.Rep.all);
  end Chunk_Size;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Dynamic_Stack);

  function New_Iterator (For_The_Stack : Dynamic_Stack) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Stack'Address);
  begin
    return Iterator (SP.Create (new Stack_Iterator (P)));
  end New_Iterator;

  procedure Purge (Obj : in out Dynamic_Stack) is
  begin
    Dynamic_Stack_Nodes.Clear (Obj.Rep.all);
  end Purge;

  procedure Add (Obj : in out Dynamic_Stack; Elem : Item) is
  begin
    Dynamic_Stack_Nodes.Append (Obj.Rep.all, Elem);
  end Add;

  function Cardinality (Obj : Dynamic_Stack) return Natural is
  begin
    return Dynamic_Stack_Nodes.Length (Obj.Rep.all);
  end Cardinality;

  function Item_At (Obj : Dynamic_Stack; Index : Positive) return Item_Ptr is
  begin
    return Dynamic_Stack_Nodes.Item_At (Obj.Rep.all, Index);
  end Item_At;

  procedure Initialize (Obj : in out Dynamic_Stack) is
  begin
    Obj.Rep := Dynamic_Stack_Nodes.Create;
  end Initialize;

  procedure Adjust (Obj : in out Dynamic_Stack) is
  begin
    Obj.Rep := Dynamic_Stack_Nodes.Create (Obj.Rep.all);
  end Adjust;

  procedure Finalize (Obj : in out Dynamic_Stack) is
    use type Dynamic_Stack_Nodes.Dyn_Node_Ref;
  begin
    if Obj.Rep /= null then
      Dynamic_Stack_Nodes.Free (Obj.Rep);
    end if;
  end Finalize;

end BC.Containers.Stacks.Dynamic;
