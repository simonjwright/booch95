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

package body BC.Containers.Stacks.Unbounded is

  function "=" (Left, Right : Unbounded_Stack) return Boolean is
    use Unbounded_Stack_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Clear (Obj : in out Unbounded_Stack) is
  begin
    Unbounded_Stack_Nodes.Clear (Obj.Rep.all);
  end Clear;

  procedure Push (Obj : in out Unbounded_Stack; Elem : Item) is
  begin
    Unbounded_Stack_Nodes.Insert (Obj.Rep.all, Elem);
  end Push;

  procedure Pop (Obj : in out Unbounded_Stack) is
  begin
    Unbounded_Stack_Nodes.Remove (Obj.Rep.all, 1);
  end Pop;

  function Depth(Obj : Unbounded_Stack) return Natural is
  begin
    return Unbounded_Stack_Nodes.Length (Obj.Rep.all);
  end Depth;

  function Is_Empty (Obj : Unbounded_Stack) return Boolean is
  begin
    return Unbounded_Stack_Nodes.Length (Obj.Rep.all) = 0;
  end Is_Empty;

  function Top (Obj : Unbounded_Stack) return Item is
  begin
    return Unbounded_Stack_Nodes.First (Obj.Rep.all);
  end Top;

  function Top (Obj : Unbounded_Stack) return Item_Ptr is
  begin
    return Unbounded_Stack_Nodes.First (Obj.Rep.all);
  end Top;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Unbounded_Stack);

  function New_Iterator (For_The_Stack : Unbounded_Stack) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Stack'Address);
  begin
    return Iterator (SP.Create (new Stack_Iterator (P)));
  end New_Iterator;

  procedure Purge (Obj : in out Unbounded_Stack) is
  begin
    Unbounded_Stack_Nodes.Clear (Obj.Rep.all);
  end Purge;

  procedure Add (Obj : in out Unbounded_Stack; Elem : Item) is
  begin
    Unbounded_Stack_Nodes.Append (Obj.Rep.all, Elem);
  end Add;

  function Cardinality (Obj : Unbounded_Stack) return Natural is
  begin
    return Unbounded_Stack_Nodes.Length (Obj.Rep.all);
  end Cardinality;

  function Item_At (Obj : Unbounded_Stack; Index : Positive) return Item_Ptr is
  begin
    return Unbounded_Stack_Nodes.Item_At (Obj.Rep.all, Index);
  end Item_At;

  procedure Initialize (Obj : in out Unbounded_Stack) is
  begin
    null;
  end Initialize;

  procedure Adjust (Obj : in out Unbounded_Stack) is
    Tmp_Ptr : Unbounded_Stack_Nodes.Unb_Node_Ref := Obj.Rep;
  begin
    Obj.Rep := new Unbounded_Stack_Nodes.Unb_Node;
    Obj.Rep.all := Unbounded_Stack_Nodes.Create (From=>Tmp_Ptr.all);
  end Adjust;

  procedure Finalize (Obj : in out Unbounded_Stack) is
  begin
    Unbounded_Stack_Nodes.Free (Obj.Rep);
  end Finalize;

end BC.Containers.Stacks.Unbounded;
