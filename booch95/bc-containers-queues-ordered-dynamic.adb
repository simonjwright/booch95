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

package body BC.Containers.Queues.Ordered.Dynamic is

  function Create (Size : Positive) return Dynamic_Ordered_Queue is
    Temp : Dynamic_Ordered_Queue;
  begin
    Temp.Rep := Dynamic_Ordered_Queue_Nodes.Create (Size);
    return Temp;
  end Create;

  procedure Clear (Q : in out Dynamic_Ordered_Queue) is
  begin
    Dynamic_Ordered_Queue_Nodes.Clear (Q.Rep.all);
  end Clear;

  procedure Append (Q : in out Dynamic_Ordered_Queue; Elem : Item) is
  begin
    for Index in 1 .. Dynamic_Ordered_Queue_Nodes.Length (Q.Rep.all)
    loop
      if Elem < Dynamic_Ordered_Queue_Nodes.Item_At (Q.Rep.all, Index)
      then
        Dynamic_Ordered_Queue_Nodes.Insert (Q.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Dynamic_Ordered_Queue_Nodes.Append (Q.Rep.all, Elem);
  end Append;

  procedure Pop (Q : in out Dynamic_Ordered_Queue) is
  begin
    Dynamic_Ordered_Queue_Nodes.Remove (Q.Rep.all, 1);
  end Pop;

  procedure Remove (Q : in out Dynamic_Ordered_Queue; From : Positive) is
  begin
    Dynamic_Ordered_Queue_Nodes.Remove (Q.Rep.all, From);
  end Remove;

  function Length (Q : Dynamic_Ordered_Queue) return Natural is
  begin
    return Dynamic_Ordered_Queue_Nodes.Length (Q.Rep.all);
  end Length;

  function Is_Empty (Q : Dynamic_Ordered_Queue) return Boolean is
  begin
    return Dynamic_Ordered_Queue_Nodes.Length (Q.Rep.all) = 0;
  end Is_Empty;

  function Front (Q : Dynamic_Ordered_Queue) return Item is
  begin
    return Dynamic_Ordered_Queue_Nodes.First (Q.Rep.all);
  end Front;

  function Location
     (Q : Dynamic_Ordered_Queue; Elem : Item) return Natural is
  begin
    return Dynamic_Ordered_Queue_Nodes.Location (Q.Rep.all, Elem);
  end Location;

  function "=" (Left, Right : Dynamic_Ordered_Queue) return Boolean is
    use Dynamic_Ordered_Queue_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Preallocate (Q : in out Dynamic_Ordered_Queue; Size : Natural) is
  begin
    Dynamic_Ordered_Queue_Nodes.Preallocate (Q.Rep.all, Size);
  end Preallocate;

  procedure Set_Chunk_Size (Q : in out Dynamic_Ordered_Queue; Size : Natural) is
  begin
    Dynamic_Ordered_Queue_Nodes.Set_Chunk_Size (Q.Rep.all, Size);
  end Set_Chunk_Size;

  function Chunk_Size (Q : Dynamic_Ordered_Queue) return Natural is
  begin
    return Dynamic_Ordered_Queue_Nodes.Chunk_Size (Q.Rep.all);
  end Chunk_Size;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Dynamic_Ordered_Queue);

  function New_Iterator
     (For_The_Queue : Dynamic_Ordered_Queue) return Iterator'Class is
    Result : Queue_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Queue'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  function Item_At
     (Q : Dynamic_Ordered_Queue; Index : Positive) return Item_Ptr is
  begin
    return Dynamic_Ordered_Queue_Nodes.Item_At (Q.Rep.all, Index);
  end Item_At;

  procedure Initialize (Q : in out Dynamic_Ordered_Queue) is
  begin
    Q.Rep := Dynamic_Ordered_Queue_Nodes.Create;
  end Initialize;

  procedure Adjust (Q : in out Dynamic_Ordered_Queue) is
  begin
    Q.Rep := Dynamic_Ordered_Queue_Nodes.Create (From => Q.Rep.all);
  end Adjust;

  procedure Finalize (Q : in out Dynamic_Ordered_Queue) is
    use type Dynamic_Ordered_Queue_Nodes.Dyn_Node_Ref;
  begin
    if Q.Rep /= null then
      Dynamic_Ordered_Queue_Nodes.Free (Q.Rep); -- does a Clear()
    end if;
  end Finalize;

  Empty_Container : Dynamic_Ordered_Queue;

  function Null_Container return Dynamic_Ordered_Queue is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Queues.Ordered.Dynamic;
