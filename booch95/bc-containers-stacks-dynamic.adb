-- Copyright (C) 1994-2001 Grady Booch, David Weller and Simon Wright.
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

  function Create (Size : Positive) return Stack is
    Temp : Stack;
  begin
    Temp.Rep := Stack_Nodes.Create (Size);
    return Temp;
  end Create;

  function "=" (Left, Right : Stack) return Boolean is
    use Stack_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Clear (S : in out Stack) is
  begin
    Stack_Nodes.Clear (S.Rep.all);
  end Clear;

  procedure Push (S : in out Stack; Elem : Item) is
  begin
    Stack_Nodes.Insert (S.Rep.all, Elem);
  end Push;

  procedure Pop (S : in out Stack) is
  begin
    Stack_Nodes.Remove (S.Rep.all, 1);
  end Pop;

  function Depth (S : in Stack) return Natural is
  begin
    return Stack_Nodes.Length (S.Rep.all);
  end Depth;

  function Is_Empty (S : Stack) return Boolean is
  begin
    return Stack_Nodes.Length (S.Rep.all) = 0;
  end Is_Empty;

  function Top (S : Stack) return Item is
  begin
    return Stack_Nodes.First (S.Rep.all);
  end Top;

  function Top (S : in Stack) return Item_Ptr is
  begin
    return Stack_Nodes.First (S.Rep.all);
  end Top;

  function Location (S : Stack; Elem : Item) return Natural is
  begin
    return Stack_Nodes.Location (S.Rep.all, Elem);
  end Location;

  procedure Preallocate (S : in out Stack; Size : Natural) is
  begin
    Stack_Nodes.Preallocate (S.Rep.all, Size);
  end Preallocate;

  procedure Set_Chunk_Size (S : in out Stack; Size : Natural) is
  begin
    Stack_Nodes.Set_Chunk_Size (S.Rep.all, Size);
  end Set_Chunk_Size;

  function Chunk_Size (S : Stack) return Natural is
  begin
    return Stack_Nodes.Chunk_Size (S.Rep.all);
  end Chunk_Size;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Stack);

  function New_Iterator (For_The_Stack : Stack) return Iterator'Class is
    Result : Stack_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Stack'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  function Item_At (S : Stack; Index : Positive) return Item_Ptr is
  begin
    return Stack_Nodes.Item_At (S.Rep.all, Index);
  end Item_At;

  procedure Add (S : in out Stack; Elem : Item) is
  begin
    Stack_Nodes.Append (S.Rep.all, Elem);
  end Add;

  procedure Remove (S : in out Stack; From : Positive) is
  begin
    Stack_Nodes.Remove (S.Rep.all, From);
  end Remove;

  procedure Initialize (S : in out Stack) is
  begin
    S.Rep := Stack_Nodes.Create;
  end Initialize;

  procedure Adjust (S : in out Stack) is
  begin
    S.Rep := Stack_Nodes.Create (S.Rep.all);
  end Adjust;

  procedure Finalize (S : in out Stack) is
    use type Stack_Nodes.Dyn_Node_Ref;
  begin
    if S.Rep /= null then
      Stack_Nodes.Free (S.Rep);
    end if;
  end Finalize;

  Empty_Container : Stack;

  function Null_Container return Stack is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Stacks.Dynamic;
