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

package body BC.Containers.Queues.Unbounded is

  procedure Clear (Obj : in out Unbounded_Queue) is
  begin
    Unbounded_Queue_Nodes.Clear (Obj.Rep.all);
  end Clear;

  procedure Append (Obj : in out Unbounded_Queue; Elem : Item) is
  begin
    Unbounded_Queue_Nodes.Append (Obj.Rep.all, Elem);
  end Append;

  procedure Pop (Obj : in out Unbounded_Queue) is
  begin
    Unbounded_Queue_Nodes.Remove (Obj.Rep.all, 1);
  end Pop;

  procedure Remove (Obj : in out Unbounded_Queue; From : Natural) is
  begin
    Unbounded_Queue_Nodes.Remove (Obj.Rep.all, From);
  end Remove;

  function Length (Obj : Unbounded_Queue) return Natural is
  begin
    return Unbounded_Queue_Nodes.Length (Obj.Rep.all);
  end Length;

  function Is_Empty (Obj : Unbounded_Queue) return Boolean is
  begin
    return Unbounded_Queue_Nodes.Length (Obj.Rep.all) = 0;
  end Is_Empty;

  function Front (Obj : Unbounded_Queue) return Item is
  begin
    return Unbounded_Queue_Nodes.First (Obj.Rep.all);
  end Front;

  function Location (Obj : Unbounded_Queue; Elem : Item) return Natural is
  begin
    return Unbounded_Queue_Nodes.Location (Obj.Rep.all, Elem);
  end Location;

  function "=" (Left, Right : Unbounded_Queue) return Boolean is
    use Unbounded_Queue_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Unbounded_Queue);

  function New_Iterator (For_The_Queue : Unbounded_Queue) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Queue'Address);
  begin
    return Iterator (SP.Create (new Queue_Iterator (P)));
  end New_Iterator;

  procedure Purge (Obj : in out Unbounded_Queue) is
  begin
    Unbounded_Queue_Nodes.Clear (Obj.Rep.all);
  end Purge;

  procedure Add (Obj : in out Unbounded_Queue; Elem : Item) is
  begin
    Unbounded_Queue_Nodes.Append (Obj.Rep.all, Elem);
  end Add;

  function Cardinality (Obj : Unbounded_Queue) return Natural is
  begin
    return Unbounded_Queue_Nodes.Length (Obj.Rep.all);
  end Cardinality;

  function Item_At (Obj : Unbounded_Queue; Index : Positive) return Item_Ptr is
  begin
    return Unbounded_Queue_Nodes.Item_At (Obj.Rep.all, Index);
  end Item_At;

  procedure Initialize (Obj : in out Unbounded_Queue) is
  begin
    null;
  end Initialize;

  procedure Adjust (Obj : in out Unbounded_Queue) is
  begin
    Obj.Rep := Unbounded_Queue_Nodes.Create (From => Obj.Rep.all);
  end Adjust;

  procedure Finalize (Obj : in out Unbounded_Queue) is
  begin
    Unbounded_Queue_Nodes.Free (Obj.Rep); -- does a Clear()
  end Finalize;

end BC.Containers.Queues.Unbounded;
