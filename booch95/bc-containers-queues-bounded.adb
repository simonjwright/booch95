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

package body BC.Containers.Queues.Bounded is

  use Bounded_Queue_Nodes;

  procedure Clear (Q : in out Bounded_Queue) is
  begin
    Bounded_Queue_Nodes.Clear (Q.Rep.all);
  end Clear;

  procedure Append (Q : in out Bounded_Queue; Elem : Item) is
  begin
    Bounded_Queue_Nodes.Append (Q.Rep.all, Elem);
  end Append;

  procedure Pop (Q : in out Bounded_Queue) is
  begin
    Bounded_Queue_Nodes.Remove (Q.Rep.all, 1);
  end Pop;

  procedure Remove (Q : in out Bounded_Queue; From : Positive) is
  begin
    Bounded_Queue_Nodes.Remove (Q.Rep.all, From);
  end Remove;

  function Available (Q: in Bounded_Queue) return Natural is
  begin
    return Bounded_Queue_Nodes.Available (Q.Rep.all);
  end Available;

  function Length  (Q : Bounded_Queue) return Natural is
  begin
    return Bounded_Queue_Nodes.Length (Q.Rep.all);
  end Length;

  function Is_Empty (Q : Bounded_Queue) return Boolean is
  begin
    return Bounded_Queue_Nodes.Length (Q.Rep.all) = 0;
  end Is_Empty;

  function Front (Q : Bounded_Queue) return Item is
  begin
    return Bounded_Queue_Nodes.First (Q.Rep.all);
  end Front;

  function Location (Q : in Bounded_Queue; Elem : Item) return Natural is
  begin
    return Bounded_Queue_Nodes.Location (Q.Rep.all, Elem);
  end Location;

  function "=" (Left, Right : Bounded_Queue) return Boolean is
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Bounded_Queue);

  function New_Iterator
     (For_The_Queue : Bounded_Queue) return Iterator'Class is
    Result : Queue_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Queue'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  function Item_At (Q : Bounded_Queue; Index : Positive) return Item_Ptr is
  begin
    return Bounded_Queue_Nodes.Item_At (Q.Rep.all, Index);
  end Item_at;

  procedure Adjust (Q : in out Bounded_Queue) is
  begin
    Q.Rep := Bounded_Queue_Nodes.Create (Q.Rep.all);
  end Adjust;

  procedure Finalize (Q : in out Bounded_Queue) is
  begin
    Free (Q.Rep);
  end Finalize;

end BC.Containers.Queues.Bounded;
