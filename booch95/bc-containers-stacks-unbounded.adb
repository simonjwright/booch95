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

package body BC.Containers.Stacks.Unbounded is

  function "=" (Left, Right : Unb_Stack) return Boolean is
    use Unb_Stack_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Clear (Obj : in out Unb_Stack) is
  begin
    Unb_Stack_Nodes.Clear (Obj.Rep.all);
  end Clear;

  procedure Push (Obj : in out Unb_Stack; Elem : Item) is
  begin
    Unb_Stack_Nodes.Insert (Obj.Rep.all, Elem);
  end Push;

  procedure Pop (Obj : in out Unb_Stack) is
  begin
    Unb_Stack_Nodes.Remove (Obj.Rep.all, 1);
  end Pop;

  function Depth(Obj : Unb_Stack) return Natural is
  begin
    return Unb_Stack_Nodes.Length (Obj.Rep.all);
  end Depth;

  function Is_Empty (Obj : Unb_Stack) return Boolean is
  begin
    return Unb_Stack_Nodes.Length (Obj.Rep.all) = 0;
  end Is_Empty;

  function Top (Obj : Unb_Stack) return Item is
  begin
    return Unb_Stack_Nodes.First (Obj.Rep.all);
  end Top;

  function Top (Obj : Unb_Stack) return Item_Ptr is
  begin
    return Unb_Stack_Nodes.First (Obj.Rep.all'access);
  end Top;

  procedure Purge (Obj : in out Unb_Stack) is
  begin
    Unb_Stack_Nodes.Clear (Obj.Rep.all);
  end Purge;

  procedure Add (Obj : in out Unb_Stack; Elem : in out Item) is
  begin
    Unb_Stack_Nodes.Append (Obj.Rep.all, Elem);
  end Add;

  function Cardinality (Obj : Unb_Stack) return Integer is
  begin
    return Unb_Stack_Nodes.Length (Obj.Rep.all);
  end Cardinality;

  function Item_At (Obj : in Unb_Stack; Index : in Natural) return Item_Ptr is
    Tobj : aliased Unb_Stack := Obj;
  begin
    return Unb_Stack_Nodes.Item_At (TObj.Rep, Index);
  end Item_At;

  procedure Initialize (Obj : in out Unb_Stack) is
  begin
    null;
  end Initialize;

  procedure Adjust (Obj : in out Unb_Stack) is
    Tmp_Ptr : Unb_Stack_Nodes.Unb_Node_Ref := Obj.Rep;
  begin
    Obj.Rep := new Unb_Stack_Nodes.Unb_Node;
    Obj.Rep.all := Unb_Stack_Nodes.Create (From=>Tmp_Ptr.all);
  end Adjust;

  procedure Finalize (Obj : in out Unb_Stack) is
  begin
    Unb_Stack_Nodes.Free (Obj.Rep);
  end Finalize;

end BC.Containers.Stacks.Unbounded;
