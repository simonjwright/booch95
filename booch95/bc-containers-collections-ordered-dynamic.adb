-- Copyright (C) 1994-2000 Grady Booch and Simon Wright.
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

package body BC.Containers.Collections.Ordered.Dynamic is

  function "=" (Left, Right : in Dynamic_Ordered_Collection) return Boolean is
    use Dynamic_Ordered_Collection_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Clear (C : in out Dynamic_Ordered_Collection) is
  begin
    Dynamic_Ordered_Collection_Nodes.Clear (C.Rep.all);
  end Clear;

  procedure Insert (C : in out Dynamic_Ordered_Collection; Elem : Item) is
  begin
    for Index in 1 .. Dynamic_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if not (Dynamic_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
              < Elem) then
        Dynamic_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Dynamic_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Insert;

  procedure Insert (C : in out Dynamic_Ordered_Collection;
                    Elem : Item;
                    Before : Positive) is
  begin
    for Index in 1 .. Dynamic_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if not (Dynamic_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
              < Elem) then
        Dynamic_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Dynamic_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Insert;

  procedure Append (C : in out Dynamic_Ordered_Collection; Elem : Item) is
  begin
    for Index in 1 .. Dynamic_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if Elem < Dynamic_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
      then
        Dynamic_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Dynamic_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Append;

  procedure Append (C : in out Dynamic_Ordered_Collection;
                    Elem : Item;
                    After : Positive) is
  begin
    for Index in 1 .. Dynamic_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if Elem < Dynamic_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
      then
        Dynamic_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Dynamic_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Append;

  procedure Remove (C : in out Dynamic_Ordered_Collection; At_Index : Positive) is
  begin
    Dynamic_Ordered_Collection_Nodes.Remove (C.Rep.all, At_Index);
  end Remove;

  procedure Replace (C : in out Dynamic_Ordered_Collection;
                     At_Index : Positive;
                     Elem : Item) is
  begin
    Dynamic_Ordered_Collection_Nodes.Remove (C.Rep.all, At_Index);
    Insert (C, Elem);
  end Replace;

  function Length (C : Dynamic_Ordered_Collection) return Natural is
  begin
    return Dynamic_Ordered_Collection_Nodes.Length (C.Rep.all);
  end Length;

  function Is_Empty (C : Dynamic_Ordered_Collection) return Boolean is
  begin
    return Dynamic_Ordered_Collection_Nodes.Length (C.Rep.all) = 0;
  end Is_Empty;

  function First (C : Dynamic_Ordered_Collection) return Item is
  begin
    return Dynamic_Ordered_Collection_Nodes.First (C.Rep.all);
  end First;

  function Last (C : Dynamic_Ordered_Collection) return Item is
  begin
    return Dynamic_Ordered_Collection_Nodes.Last (C.Rep.all);
  end Last;

  function Item_At
     (C : Dynamic_Ordered_Collection; At_Index : Positive) return Item is
  begin
    return Item_At (C, At_Index).all;
  end Item_At;

  function Location
     (C : Dynamic_Ordered_Collection; Elem : Item) return Natural is
  begin
    return Dynamic_Ordered_Collection_Nodes.Location (C.Rep.all, Elem);
  end Location;

  function Create (Size : Positive) return Dynamic_Ordered_Collection is
    Temp : Dynamic_Ordered_Collection;
  begin
    Temp.Rep := Dynamic_Ordered_Collection_Nodes.Create (Size);
    return Temp;
  end Create;

  procedure Preallocate
     (C : in out Dynamic_Ordered_Collection; Size : Natural) is
  begin
    Dynamic_Ordered_Collection_Nodes.Preallocate (C.Rep.all, Size);
  end Preallocate;

  procedure Set_Chunk_Size
     (C : in out Dynamic_Ordered_Collection; Size : Natural) is
  begin
    Dynamic_Ordered_Collection_Nodes.Set_Chunk_Size (C.Rep.all, Size);
  end Set_Chunk_Size;

  function Chunk_Size (C : Dynamic_Ordered_Collection) return Natural is
  begin
    return Dynamic_Ordered_Collection_Nodes.Chunk_Size (C.Rep.all);
  end Chunk_Size;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Dynamic_Ordered_Collection);

  function New_Iterator
     (For_The_Collection : Dynamic_Ordered_Collection) return Iterator'Class is
    Result : Collection_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Collection'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  function Item_At
     (C : Dynamic_Ordered_Collection; Index : Positive) return Item_Ptr is
  begin
    return Dynamic_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index);
  end Item_At;

  procedure Initialize (C : in out Dynamic_Ordered_Collection) is
  begin
    C.Rep := Dynamic_Ordered_Collection_Nodes.Create;
  end Initialize;

  procedure Adjust (C : in out Dynamic_Ordered_Collection) is
  begin
    C.Rep := Dynamic_Ordered_Collection_Nodes.Create (From => C.Rep.all);
  end Adjust;

  procedure Finalize (C : in out Dynamic_Ordered_Collection) is
  begin
    Dynamic_Ordered_Collection_Nodes.Free (C.Rep);
  end Finalize;

  Empty_Container : Dynamic_Ordered_Collection;

  function Null_Container return Dynamic_Ordered_Collection is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Collections.Ordered.Dynamic;
