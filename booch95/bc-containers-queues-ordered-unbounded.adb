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

package body BC.Containers.Queues.Ordered.Unbounded is

  procedure Clear (Q : in out Unbounded_Ordered_Queue) is
  begin
    Unbounded_Ordered_Queue_Nodes.Clear (Q.Rep.all);
  end Clear;

  procedure Append (Q : in out Unbounded_Ordered_Queue; Elem : Item) is
  begin
    for Index in 1 .. Unbounded_Ordered_Queue_Nodes.Length (Q.Rep.all)
    loop
      if Elem < Unbounded_Ordered_Queue_Nodes.Item_At (Q.Rep.all, Index)
      then
        Unbounded_Ordered_Queue_Nodes.Insert (Q.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Unbounded_Ordered_Queue_Nodes.Append (Q.Rep.all, Elem);
  end Append;

  procedure Pop (Q : in out Unbounded_Ordered_Queue) is
  begin
    Unbounded_Ordered_Queue_Nodes.Remove (Q.Rep.all, 1);
  end Pop;

  procedure Remove (Q : in out Unbounded_Ordered_Queue; From : Positive) is
  begin
    Unbounded_Ordered_Queue_Nodes.Remove (Q.Rep.all, From);
  end Remove;

  function Length (Q : Unbounded_Ordered_Queue) return Natural is
  begin
    return Unbounded_Ordered_Queue_Nodes.Length (Q.Rep.all);
  end Length;

  function Is_Empty (Q : Unbounded_Ordered_Queue) return Boolean is
  begin
    return Unbounded_Ordered_Queue_Nodes.Length (Q.Rep.all) = 0;
  end Is_Empty;

  function Front (Q : Unbounded_Ordered_Queue) return Item is
  begin
    return Unbounded_Ordered_Queue_Nodes.First (Q.Rep.all);
  end Front;

  function Location
     (Q : Unbounded_Ordered_Queue; Elem : Item) return Natural is
  begin
    return Unbounded_Ordered_Queue_Nodes.Location (Q.Rep.all, Elem);
  end Location;

  function "=" (Left, Right : Unbounded_Ordered_Queue) return Boolean is
    use Unbounded_Ordered_Queue_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Unbounded_Ordered_Queue);

  function New_Iterator
     (For_The_Queue : Unbounded_Ordered_Queue) return Iterator'Class is
    Result : Queue_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Queue'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  function Item_At
     (Q : Unbounded_Ordered_Queue; Index : Positive) return Item_Ptr is
  begin
    return Unbounded_Ordered_Queue_Nodes.Item_At (Q.Rep.all, Index);
  end Item_At;

  procedure Initialize (Q : in out Unbounded_Ordered_Queue) is
  begin
    null;
  end Initialize;

  procedure Adjust (Q : in out Unbounded_Ordered_Queue) is
  begin
    Q.Rep := Unbounded_Ordered_Queue_Nodes.Create (From => Q.Rep.all);
  end Adjust;

  procedure Finalize (Q : in out Unbounded_Ordered_Queue) is
  begin
    Unbounded_Ordered_Queue_Nodes.Free (Q.Rep); -- does a Clear()
  end Finalize;

  Empty_Container : Unbounded_Ordered_Queue;

  function Null_Container return Unbounded_Ordered_Queue is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Queues.Ordered.Unbounded;
