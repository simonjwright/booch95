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

package body BC.Containers.Deques.Bounded is

  procedure Clear (D : in out Bounded_Deque) is
  begin
    Bounded_Deque_Nodes.Clear (D.Rep.all);
  end Clear;

  procedure Append (D : in out Bounded_Deque;
                    Elem : Item;
                    Location : Deque_End := Back) is
  begin
    if Location = Back then
      Bounded_Deque_Nodes.Append (D.Rep.all, Elem);
    else
      Bounded_Deque_Nodes.Insert (D.Rep.all, Elem);
    end if;
  end Append;

  procedure Pop (D : in out Bounded_Deque; Location : Deque_End := Front) is
  begin
    if Location = Front then
      Bounded_Deque_Nodes.Remove (D.Rep.all, 1);
    else
      Bounded_Deque_Nodes.Remove (D.Rep.all,
                                  Bounded_Deque_Nodes.Length (D.Rep.all));
    end if;
  end Pop;

  procedure Remove (D : in out Bounded_Deque; From : Positive) is
  begin
    Bounded_Deque_Nodes.Remove (D.Rep.all, From);
  end Remove;

  function Available (D: in Bounded_Deque) return Natural is
  begin
    return Bounded_Deque_Nodes.Available (D.Rep.all);
  end Available;

  function Length (D : Bounded_Deque) return Natural is
  begin
    return Bounded_Deque_Nodes.Length (D.Rep.all);
  end Length;

  function Is_Empty (D : Bounded_Deque) return Boolean is
  begin
    return Bounded_Deque_Nodes.Length (D.Rep.all) = 0;
  end Is_Empty;

  function Front (D : Bounded_Deque) return Item is
  begin
    return Bounded_Deque_Nodes.First (D.Rep.all);
  end Front;

  function Back (D : Bounded_Deque) return Item is
  begin
    return Bounded_Deque_Nodes.Last (D.Rep.all);
  end Back;

  function Location (D : Bounded_Deque; Elem : Item) return Natural is
  begin
    return Bounded_Deque_Nodes.Location (D.Rep.all, Elem);
  end Location;

  function "=" (Left, Right : Bounded_Deque) return Boolean is
    use Bounded_Deque_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Bounded_Deque);

  function New_Iterator
     (For_The_Deque : Bounded_Deque) return Iterator'Class is
    Result : Deque_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Deque'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  function Item_At (D : Bounded_Deque; Index : Positive) return Item_Ptr is
  begin
    return Bounded_Deque_Nodes.Item_At (D.Rep.all, Index);
  end Item_At;

  procedure Adjust (D : in out Bounded_Deque) is
  begin
    D.Rep := Bounded_Deque_Nodes.Create (From => D.Rep.all);
  end Adjust;

  procedure Finalize (D : in out Bounded_Deque) is
  begin
    Bounded_Deque_Nodes.Free (D.Rep); -- does a Clear()
  end Finalize;

  Empty_Container : Bounded_Deque;

  function Null_Container return Bounded_Deque is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Deques.Bounded;
