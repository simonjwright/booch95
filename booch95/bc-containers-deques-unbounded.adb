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

package body BC.Containers.Deques.Unbounded is

  procedure Clear (D : in out Unbounded_Deque) is
  begin
    Unbounded_Deque_Nodes.Clear (D.Rep.all);
  end Clear;

  procedure Append (D : in out Unbounded_Deque; 
		    Elem : Item;
		    Location : Deque_End := Back) is
  begin
    if Location = Back then
      Unbounded_Deque_Nodes.Append (D.Rep.all, Elem);
    else
      Unbounded_Deque_Nodes.Insert (D.Rep.all, Elem);
    end if;
  end Append;

  procedure Pop (D : in out Unbounded_Deque; Location : Deque_End := Front) is
  begin
    if Location = Front then
      Unbounded_Deque_Nodes.Remove (D.Rep.all, 1);
    else
      Unbounded_Deque_Nodes.Remove (D.Rep.all,
				    Unbounded_Deque_Nodes.Length (D.Rep.all));
    end if;
  end Pop;

  procedure Remove (D : in out Unbounded_Deque; From : Positive) is
  begin
    Unbounded_Deque_Nodes.Remove (D.Rep.all, From);
  end Remove;

  function Length (D : Unbounded_Deque) return Natural is
  begin
    return Unbounded_Deque_Nodes.Length (D.Rep.all);
  end Length;

  function Is_Empty (D : Unbounded_Deque) return Boolean is
  begin
    return Unbounded_Deque_Nodes.Length (D.Rep.all) = 0;
  end Is_Empty;

  function Front (D : Unbounded_Deque) return Item is
  begin
    return Unbounded_Deque_Nodes.First (D.Rep.all);
  end Front;

  function Back (D : Unbounded_Deque) return Item is
  begin
    return Unbounded_Deque_Nodes.Last (D.Rep.all);
  end Back;

  function Location (D : Unbounded_Deque; Elem : Item) return Natural is
  begin
    return Unbounded_Deque_Nodes.Location (D.Rep.all, Elem);
  end Location;

  function "=" (Left, Right : Unbounded_Deque) return Boolean is
    use Unbounded_Deque_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Unbounded_Deque);

  function New_Iterator (For_The_Deque : Unbounded_Deque) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Deque'Address);
  begin
    return Iterator (SP.Create (new Deque_Iterator (P)));
  end New_Iterator;

  function Item_At (D : Unbounded_Deque; Index : Positive) return Item_Ptr is
  begin
    return Unbounded_Deque_Nodes.Item_At (D.Rep.all, Index);
  end Item_At;

  procedure Initialize (D : in out Unbounded_Deque) is
  begin
    null;
  end Initialize;

  procedure Adjust (D : in out Unbounded_Deque) is
  begin
    D.Rep := Unbounded_Deque_Nodes.Create (From => D.Rep.all);
  end Adjust;

  procedure Finalize (D : in out Unbounded_Deque) is
  begin
    Unbounded_Deque_Nodes.Free (D.Rep); -- does a Clear()
  end Finalize;

end BC.Containers.Deques.Unbounded;
