-- Copyright (C) 1994-2000 Grady Booch, David Weller and Simon Wright.
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

  procedure Clear (Q : in out Unbounded_Queue) is
  begin
    Unbounded_Queue_Nodes.Clear (Q.Rep.all);
  end Clear;

  procedure Append (Q : in out Unbounded_Queue; Elem : Item) is
  begin
    Unbounded_Queue_Nodes.Append (Q.Rep.all, Elem);
  end Append;

  procedure Pop (Q : in out Unbounded_Queue) is
  begin
    Unbounded_Queue_Nodes.Remove (Q.Rep.all, 1);
  end Pop;

  procedure Remove (Q : in out Unbounded_Queue; From : Positive) is
  begin
    Unbounded_Queue_Nodes.Remove (Q.Rep.all, From);
  end Remove;

  function Length (Q : Unbounded_Queue) return Natural is
  begin
    return Unbounded_Queue_Nodes.Length (Q.Rep.all);
  end Length;

  function Is_Empty (Q : Unbounded_Queue) return Boolean is
  begin
    return Unbounded_Queue_Nodes.Length (Q.Rep.all) = 0;
  end Is_Empty;

  function Front (Q : Unbounded_Queue) return Item is
  begin
    return Unbounded_Queue_Nodes.First (Q.Rep.all);
  end Front;

  function Location (Q : Unbounded_Queue; Elem : Item) return Natural is
  begin
    return Unbounded_Queue_Nodes.Location (Q.Rep.all, Elem);
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

  function Item_At (Q : Unbounded_Queue; Index : Positive) return Item_Ptr is
  begin
    return Unbounded_Queue_Nodes.Item_At (Q.Rep.all, Index);
  end Item_At;

  procedure Initialize (Q : in out Unbounded_Queue) is
  begin
    null;
  end Initialize;

  procedure Adjust (Q : in out Unbounded_Queue) is
  begin
    Q.Rep := Unbounded_Queue_Nodes.Create (From => Q.Rep.all);
  end Adjust;

  procedure Finalize (Q : in out Unbounded_Queue) is
  begin
    Unbounded_Queue_Nodes.Free (Q.Rep); -- does a Clear()
  end Finalize;

end BC.Containers.Queues.Unbounded;
