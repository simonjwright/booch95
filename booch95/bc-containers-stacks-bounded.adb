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

package body BC.Containers.Stacks.Bounded is

  procedure Clear (S : in out Bounded_Stack) is
  begin
    Bounded_Stack_Nodes.Clear (S.Rep.all);
  end Clear;

  procedure Push (S : in out Bounded_Stack; Elem : Item) is
  begin
    Bounded_Stack_Nodes.Insert (S.Rep.all, Elem);
  end Push;

  procedure Pop (S : in out Bounded_Stack) is
  begin
    Bounded_Stack_Nodes.Remove (S.Rep.all, 1);
  end Pop;

  function Available (S: in Bounded_Stack) return Natural is
  begin
    return Bounded_Stack_Nodes.Available (S.Rep.all);
  end Available;

  function Depth (S : Bounded_Stack) return Natural is
  begin
    return Bounded_Stack_Nodes.Length (S.Rep.all);
  end Depth;

  function Is_Empty (S : Bounded_Stack) return Boolean is
  begin
    return Bounded_Stack_Nodes.Length (S.Rep.all) = 0;
  end Is_Empty;

  function Top (S : Bounded_Stack) return Item is
  begin
    return Bounded_Stack_Nodes.First (S.Rep.all);
  end Top;

  function "=" (Left, Right : Bounded_Stack) return Boolean is
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Bounded_Stack);

  function New_Iterator (For_The_Stack : Bounded_Stack) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Stack'Address);
  begin
    return Iterator (SP.Create (new Stack_Iterator (P)));
  end New_Iterator;

  function Cardinality (S : Bounded_Stack) return Natural is
  begin
    return Bounded_Stack_Nodes.Length (S.Rep.all);
  end Cardinality;

  function Item_At
     (S : Bounded_Stack; Index : Positive) return Item_Ptr is
  begin
    return Bounded_Stack_Nodes.Item_At (S.Rep.all, Index);
  end Item_at;

  procedure Purge (S : in out Bounded_Stack) is
  begin
    Bounded_Stack_Nodes.Clear (S.Rep.all);
  end Purge;

  procedure Add (S : in out Bounded_Stack; Elem : Item) is
  begin
    Bounded_Stack_Nodes.Append (S.Rep.all, Elem);
  end Add;

  procedure Remove (S : in out Bounded_Stack; From : Positive) is
  begin
    Bounded_Stack_Nodes.Remove (S.Rep.all, From);
  end Remove;

  procedure Adjust (S : in out Bounded_Stack) is
  begin
    S.Rep := Bounded_Stack_Nodes.Create (S.Rep.all);
  end Adjust;

  procedure Finalize (S : in out Bounded_Stack) is
  begin
    Free (S.Rep);
  end Finalize;

end BC.Containers.Stacks.Bounded;
