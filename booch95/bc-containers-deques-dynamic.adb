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

package body BC.Containers.Deques.Dynamic is

  function Create (Size : Positive) return Deque is
    Temp : Deque;
  begin
    Temp.Rep := Deque_Nodes.Create (Size);
    return Temp;
  end Create;

  procedure Clear (D : in out Deque) is
  begin
    Deque_Nodes.Clear (D.Rep.all);
  end Clear;

  procedure Append (D : in out Deque;
                    Elem : Item;
                    Location : Deque_End := Back) is
  begin
    if Location = Back then
      Deque_Nodes.Append (D.Rep.all, Elem);
    else
      Deque_Nodes.Insert (D.Rep.all, Elem);
    end if;
  end Append;

  procedure Pop (D : in out Deque; Location : Deque_End := Front) is
  begin
    if Location = Front then
      Deque_Nodes.Remove (D.Rep.all, 1);
    else
      Deque_Nodes.Remove (D.Rep.all,
                          Deque_Nodes.Length (D.Rep.all));
    end if;
  end Pop;

  procedure Remove (D : in out Deque; From : Positive) is
  begin
    Deque_Nodes.Remove (D.Rep.all, From);
  end Remove;

  function Length (D : Deque) return Natural is
  begin
    return Deque_Nodes.Length (D.Rep.all);
  end Length;

  function Is_Empty (D : Deque) return Boolean is
  begin
    return Deque_Nodes.Length (D.Rep.all) = 0;
  end Is_Empty;

  function Front (D : Deque) return Item is
  begin
    return Deque_Nodes.First (D.Rep.all);
  end Front;

  function Back (D : Deque) return Item is
  begin
    return Deque_Nodes.Last (D.Rep.all);
  end Back;

  function Location (D : Deque; Elem : Item) return Natural is
  begin
    return Deque_Nodes.Location (D.Rep.all, Elem);
  end Location;

  function "=" (Left, Right : Deque) return Boolean is
    use Deque_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Preallocate (D : in out Deque; Size : Natural) is
  begin
    Deque_Nodes.Preallocate (D.Rep.all, Size);
  end Preallocate;

  procedure Set_Chunk_Size (D : in out Deque; Size : Natural) is
  begin
    Deque_Nodes.Set_Chunk_Size (D.Rep.all, Size);
  end Set_Chunk_Size;

  function Chunk_Size (D : Deque) return Natural is
  begin
    return Deque_Nodes.Chunk_Size (D.Rep.all);
  end Chunk_Size;

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
    return Deque_Nodes.Item_At (D.Rep.all, Index);
  end Item_At;

  procedure Initialize (D : in out Deque) is
  begin
    D.Rep := Deque_Nodes.Create;
  end Initialize;

  procedure Adjust (D : in out Deque) is
  begin
    D.Rep := Deque_Nodes.Create (From => D.Rep.all);
  end Adjust;

  procedure Finalize (D : in out Deque) is
    use type Deque_Nodes.Dyn_Node_Ref;
  begin
    if D.Rep /= null then
      Deque_Nodes.Free (D.Rep); -- does a Clear()
    end if;
  end Finalize;

  Empty_Container : Deque;

  function Null_Container return Deque is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Deques.Dynamic;
