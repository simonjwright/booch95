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

package body BC.Containers.Deques.Unbounded is

  procedure Clear (D : in out Deque) is
  begin
    Deque_Nodes.Clear (D.Rep);
  end Clear;

  procedure Append (D : in out Deque;
                    Elem : Item;
                    Location : Deque_End := Back) is
  begin
    if Location = Back then
      Deque_Nodes.Append (D.Rep, Elem);
    else
      Deque_Nodes.Insert (D.Rep, Elem);
    end if;
  end Append;

  procedure Pop (D : in out Deque; Location : Deque_End := Front) is
  begin
    if Location = Front then
      Deque_Nodes.Remove (D.Rep, 1);
    else
      Deque_Nodes.Remove (D.Rep,
                          Deque_Nodes.Length (D.Rep));
    end if;
  end Pop;

  procedure Remove (D : in out Deque; From : Positive) is
  begin
    Deque_Nodes.Remove (D.Rep, From);
  end Remove;

  function Length (D : Deque) return Natural is
  begin
    return Deque_Nodes.Length (D.Rep);
  end Length;

  function Is_Empty (D : Deque) return Boolean is
  begin
    return Deque_Nodes.Length (D.Rep) = 0;
  end Is_Empty;

  function Front (D : Deque) return Item is
  begin
    return Deque_Nodes.First (D.Rep);
  end Front;

  function Back (D : Deque) return Item is
  begin
    return Deque_Nodes.Last (D.Rep);
  end Back;

  function Location (D : Deque; Elem : Item) return Natural is
  begin
    return Deque_Nodes.Location (D.Rep, Elem);
  end Location;

  function "=" (Left, Right : Deque) return Boolean is
    use Deque_Nodes;
  begin
    return Left.Rep = Right.Rep;
  end "=";

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Deque);

  function New_Iterator
     (For_The_Deque : Deque) return Iterator'Class is
    Result : Deque_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Deque'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  function Item_At (D : Deque; Index : Positive) return Item_Ptr is
  begin
    return Deque_Nodes.Item_At (D.Rep, Index);
  end Item_At;

  Empty_Container : Deque;
  pragma Warnings (Off, Empty_Container);

  function Null_Container return Deque is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Deques.Unbounded;
