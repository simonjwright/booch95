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

package body BC.Containers.Collections.Ordered.Unbounded is

  function "=" (Left, Right : in Unbounded_Ordered_Collection) return Boolean is
    use Unbounded_Ordered_Collection_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Clear (C : in out Unbounded_Ordered_Collection) is
  begin
    Unbounded_Ordered_Collection_Nodes.Clear (C.Rep.all);
  end Clear;

  procedure Insert (C : in out Unbounded_Ordered_Collection; Elem : Item) is
  begin
    for Index in 1 .. Unbounded_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if not (Unbounded_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
              < Elem) then
        Unbounded_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Unbounded_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Insert;

  procedure Insert (C : in out Unbounded_Ordered_Collection;
                    Elem : Item;
                    Before : Positive) is
  begin
    for Index in 1 .. Unbounded_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if not (Unbounded_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
              < Elem) then
        Unbounded_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Unbounded_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Insert;

  procedure Append (C : in out Unbounded_Ordered_Collection; Elem : Item) is
  begin
    for Index in 1 .. Unbounded_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if Elem < Unbounded_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
      then
        Unbounded_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Unbounded_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Append;

  procedure Append (C : in out Unbounded_Ordered_Collection;
                    Elem : Item;
                    After : Positive) is
  begin
    for Index in 1 .. Unbounded_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if Elem < Unbounded_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
      then
        Unbounded_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Unbounded_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Append;

  procedure Remove (C : in out Unbounded_Ordered_Collection; At_Index : Positive) is
  begin
    Unbounded_Ordered_Collection_Nodes.Remove (C.Rep.all, At_Index);
  end Remove;

  procedure Replace (C : in out Unbounded_Ordered_Collection;
                     At_Index : Positive;
                     Elem : Item) is
  begin
    Unbounded_Ordered_Collection_Nodes.Remove (C.Rep.all, At_Index);
    Insert (C, Elem);
  end Replace;

  function Length (C : Unbounded_Ordered_Collection) return Natural is
  begin
    return Unbounded_Ordered_Collection_Nodes.Length (C.Rep.all);
  end Length;

  function Is_Empty (C : Unbounded_Ordered_Collection) return Boolean is
  begin
    return Unbounded_Ordered_Collection_Nodes.Length (C.Rep.all) = 0;
  end Is_Empty;

  function First (C : Unbounded_Ordered_Collection) return Item is
  begin
    return Unbounded_Ordered_Collection_Nodes.First (C.Rep.all);
  end First;

  function Last (C : Unbounded_Ordered_Collection) return Item is
  begin
    return Unbounded_Ordered_Collection_Nodes.Last (C.Rep.all);
  end Last;

  function Item_At
     (C : Unbounded_Ordered_Collection; At_Index : Positive) return Item is
  begin
    return Item_At (C, At_Index).all;
  end Item_At;

  function Location
     (C : Unbounded_Ordered_Collection; Elem : Item) return Natural is
  begin
    return Unbounded_Ordered_Collection_Nodes.Location (C.Rep.all, Elem);
  end Location;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Unbounded_Ordered_Collection);

  function New_Iterator
     (For_The_Collection : Unbounded_Ordered_Collection) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Collection'Address);
  begin
    return Iterator (SP.Create (new Collection_Iterator (P)));
  end New_Iterator;

  function Item_At
     (C : Unbounded_Ordered_Collection; Index : Positive) return Item_Ptr is
  begin
    return Unbounded_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index);
  end Item_At;

  procedure Initialize (C : in out Unbounded_Ordered_Collection) is
  begin
    null;
  end Initialize;

  procedure Adjust (C : in out Unbounded_Ordered_Collection) is
  begin
    C.Rep := Unbounded_Ordered_Collection_Nodes.Create (From => C.Rep.all);
  end Adjust;

  procedure Finalize (C : in out Unbounded_Ordered_Collection) is
  begin
    Unbounded_Ordered_Collection_Nodes.Free (C.Rep);
  end Finalize;

end BC.Containers.Collections.Ordered.Unbounded;
