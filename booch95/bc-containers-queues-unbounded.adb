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

package body BC.Containers.Queues.Unbounded is

  procedure Clear (Obj : in out Unb_Queue) is
  begin
    Unb_Queue_Nodes.Clear (Obj.Rep.all);
  end Clear;

  procedure Append (Obj : in out Unb_Queue; Elem : Item) is
  begin
    Unb_Queue_Nodes.Append (Obj.Rep.all, Elem);
  end Append;

  procedure Pop (Obj : in out Unb_Queue) is
  begin
    Unb_Queue_Nodes.Remove (Obj.Rep.all, 1);
  end Pop;

  procedure Remove (Obj : in out Unb_Queue; From : Natural) is
  begin
    Unb_Queue_Nodes.Remove (Obj.Rep.all, From);
  end Remove;

  function Length (Obj : Unb_Queue) return Natural is
  begin
    return Unb_Queue_Nodes.Length (Obj.Rep.all);
  end Length;

  function Is_Empty (Obj : Unb_Queue) return Boolean is
  begin
    return Unb_Queue_Nodes.Length (Obj.Rep.all) = 0;
  end Is_Empty;

  function Front (Obj : Unb_Queue) return Item is
  begin
    return Unb_Queue_Nodes.First (Obj.Rep.all);
  end Front;

  function Front (Obj : Unb_Queue) return Item_Ptr is
  begin
    return Unb_Queue_Nodes.First (Obj.Rep.all'access);
  end Front;

  function Location (Obj : Unb_Queue; Elem : Item) return Natural is
  begin
    return Unb_Queue_Nodes.Location (Obj.Rep, Elem);
  end Location;

  function "=" (Left, Right : Unb_Queue) return Boolean is
    use Unb_Queue_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Purge (Obj : in out Unb_Queue) is
  begin
    Unb_Queue_Nodes.Clear (Obj.Rep.all);
  end Purge;

  procedure Add (Obj : in out Unb_Queue; Elem : in out Item) is
  begin
    Unb_Queue_Nodes.Append (Obj.Rep.all, Elem);
  end Add;

  function Cardinality (Obj : Unb_Queue) return Integer is
  begin
    return Unb_Queue_Nodes.Length (Obj.Rep.all);
  end Cardinality;

  function Item_At (Obj : in Unb_Queue; Index : in Natural) return Item_Ptr is
    Tobj : aliased Unb_Queue := Obj;
  begin
    return Unb_Queue_Nodes.Item_At (TObj.Rep, Index);
  end Item_At;

  procedure Initialize (Obj : in out Unb_Queue) is
  begin
    null;
  end Initialize;

  procedure Adjust (Obj : in out Unb_Queue) is
    Tmp_Ptr : Unb_Queue_Nodes.Unb_Node_Ref := Obj.Rep;
  begin
    Obj.Rep := new Unb_Queue_Nodes.Unb_Node;
    Obj.Rep.all := Unb_Queue_Nodes.Create(From=>Tmp_Ptr.all);
  end Adjust;

  procedure Finalize (Obj : in out Unb_Queue) is
  begin
    Unb_Queue_Nodes.Free (Obj.Rep); -- does a Clear()
  end Finalize;

end BC.Containers.Queues.Unbounded;
