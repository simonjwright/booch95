-- Copyright (C) 1994-1999 Grady Booch, David Weller and Simon Wright.
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

package body BC.Containers.Queues.Dynamic is

  function Create (Size : Positive) return Dynamic_Queue is
    Temp : Dynamic_Queue;
  begin
    Temp.Rep := Dynamic_Queue_Nodes.Create (Size);
    return Temp;
  end Create;

  function "=" (Left, Right : Dynamic_Queue) return Boolean is
    use Dynamic_Queue_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Clear (Q : in out Dynamic_Queue) is
  begin
    Dynamic_Queue_Nodes.Clear (Q.Rep.all);
  end Clear;

  procedure Append (Q : in out Dynamic_Queue; Elem : Item) is
  begin
    Dynamic_Queue_Nodes.Append (Q.Rep.all, Elem);
  end Append;

  procedure Pop (Q : in out Dynamic_Queue) is
  begin
    Dynamic_Queue_Nodes.Remove (Q.Rep.all, 1);
  end Pop;

  procedure Remove (Q : in out Dynamic_Queue; From : Positive) is
  begin
    Dynamic_Queue_Nodes.Remove (Q.Rep.all, From);
  end Remove;

  function Length (Q : Dynamic_Queue) return Natural is
  begin
    return Dynamic_Queue_Nodes.Length (Q.Rep.all);
  end Length;

  function Is_Empty (Q : Dynamic_Queue) return Boolean is
  begin
    return Dynamic_Queue_Nodes.Length (Q.Rep.all) = 0;
  end Is_Empty;

  function Front (Q : Dynamic_Queue) return Item is
  begin
    return Dynamic_Queue_Nodes.First (Q.Rep.all);
  end Front;

  function Location (Q : Dynamic_Queue; Elem : Item) return Natural is
  begin
    return Dynamic_Queue_Nodes.Location (Q.Rep.all, Elem);
  end Location;

  procedure Preallocate (Q : in out Dynamic_Queue; Size : Natural) is
  begin
    Dynamic_Queue_Nodes.Preallocate (Q.Rep.all, Size);
  end Preallocate;

  procedure Set_Chunk_Size (Q : in out Dynamic_Queue; Size : Natural) is
  begin
    Dynamic_Queue_Nodes.Set_Chunk_Size (Q.Rep.all, Size);
  end Set_Chunk_Size;

  function Chunk_Size (Q : Dynamic_Queue) return Natural is
  begin
    return Dynamic_Queue_Nodes.Chunk_Size (Q.Rep.all);
  end Chunk_Size;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Dynamic_Queue);

  function New_Iterator (For_The_Queue : Dynamic_Queue) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Queue'Address);
  begin
    return Iterator (SP.Create (new Queue_Iterator (P)));
  end New_Iterator;

 function Cardinality (Q : Dynamic_Queue) return Natural is
  begin
    return Dynamic_Queue_Nodes.Length (Q.Rep.all);
  end Cardinality;

  function Item_At (Q : Dynamic_Queue; Index : Positive) return Item_Ptr is
  begin
    return Dynamic_Queue_Nodes.Item_At (Q.Rep.all, Index);
  end Item_At;

  procedure Purge (Q : in out Dynamic_Queue) is
  begin
    Dynamic_Queue_Nodes.Clear (Q.Rep.all);
  end Purge;

   procedure Initialize (Q : in out Dynamic_Queue) is
  begin
    Q.Rep := Dynamic_Queue_Nodes.Create;
  end Initialize;

  procedure Adjust (Q : in out Dynamic_Queue) is
  begin
    Q.Rep := Dynamic_Queue_Nodes.Create (Q.Rep.all);
  end Adjust;

  procedure Finalize (Q : in out Dynamic_Queue) is
    use type Dynamic_Queue_Nodes.Dyn_Node_Ref;
  begin
    if Q.Rep /= null then
      Dynamic_Queue_Nodes.Free (Q.Rep);
    end if;
  end Finalize;

end BC.Containers.Queues.Dynamic;
