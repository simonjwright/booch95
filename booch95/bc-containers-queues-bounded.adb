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

package body BC.Containers.Queues.Bounded is

  use Queue_Nodes;

  procedure Clear (Q : in out Queue) is
  begin
    Queue_Nodes.Clear (Q.Rep.all);
  end Clear;

  procedure Append (Q : in out Queue; Elem : Item) is
  begin
    Queue_Nodes.Append (Q.Rep.all, Elem);
  end Append;

  procedure Pop (Q : in out Queue) is
  begin
    Queue_Nodes.Remove (Q.Rep.all, 1);
  end Pop;

  procedure Remove (Q : in out Queue; From : Positive) is
  begin
    Queue_Nodes.Remove (Q.Rep.all, From);
  end Remove;

  function Available (Q : in Queue) return Natural is
  begin
    return Queue_Nodes.Available (Q.Rep.all);
  end Available;

  function Length  (Q : Queue) return Natural is
  begin
    return Queue_Nodes.Length (Q.Rep.all);
  end Length;

  function Is_Empty (Q : Queue) return Boolean is
  begin
    return Queue_Nodes.Length (Q.Rep.all) = 0;
  end Is_Empty;

  function Front (Q : Queue) return Item is
  begin
    return Queue_Nodes.First (Q.Rep.all);
  end Front;

  function Location (Q : in Queue; Elem : Item) return Natural is
  begin
    return Queue_Nodes.Location (Q.Rep.all, Elem);
  end Location;

  function "=" (Left, Right : Queue) return Boolean is
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Queue);

  function New_Iterator
     (For_The_Queue : Queue) return Iterator'Class is
    Result : Queue_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Queue'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  function Item_At (Q : Queue; Index : Positive) return Item_Ptr is
  begin
    return Queue_Nodes.Item_At (Q.Rep.all, Index);
  end Item_at;

  procedure Adjust (Q : in out Queue) is
  begin
    Q.Rep := Queue_Nodes.Create (Q.Rep.all);
  end Adjust;

  procedure Finalize (Q : in out Queue) is
  begin
    Free (Q.Rep);
  end Finalize;

  Empty_Container : Queue;

  function Null_Container return Queue is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Queues.Bounded;
