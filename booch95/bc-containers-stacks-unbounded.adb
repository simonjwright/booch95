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

package body BC.Containers.Stacks.Unbounded is

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

  function Depth(S : Stack) return Natural is
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

  function Top (S : Stack) return Item_Ptr is
  begin
    return Stack_Nodes.First (S.Rep.all);
  end Top;

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
    null;
  end Initialize;

  procedure Adjust (S : in out Stack) is
  begin
    S.Rep := Stack_Nodes.Create (From => S.Rep.all);
  end Adjust;

  procedure Finalize (S : in out Stack) is
  begin
    Stack_Nodes.Free (S.Rep);
  end Finalize;

  Empty_Container : Stack;

  function Null_Container return Stack is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Stacks.Unbounded;
