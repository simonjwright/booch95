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

package body BC.Containers.Collections.Ordered.Bounded is

  function "=" (Left, Right : in Bounded_Ordered_Collection) return Boolean is
    use Bounded_Ordered_Collection_Nodes;
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Clear (C : in out Bounded_Ordered_Collection) is
  begin
    Bounded_Ordered_Collection_Nodes.Clear (C.Rep.all);
  end Clear;

  procedure Insert (C : in out Bounded_Ordered_Collection; Elem : Item) is
  begin
    for Index in 1 .. Bounded_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if not (Bounded_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
              < Elem) then
        Bounded_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Bounded_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Insert;

  procedure Insert (C : in out Bounded_Ordered_Collection;
                    Elem : Item;
                    Before : Positive) is
  begin
    for Index in 1 .. Bounded_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if not (Bounded_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
              < Elem) then
        Bounded_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Bounded_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Insert;

  procedure Append (C : in out Bounded_Ordered_Collection; Elem : Item) is
  begin
    for Index in 1 .. Bounded_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if Elem < Bounded_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
      then
        Bounded_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Bounded_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Append;

  procedure Append (C : in out Bounded_Ordered_Collection;
                    Elem : Item;
                    After : Positive) is
  begin
    for Index in 1 .. Bounded_Ordered_Collection_Nodes.Length (C.Rep.all)
    loop
      if Elem < Bounded_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index)
      then
        Bounded_Ordered_Collection_Nodes.Insert (C.Rep.all, Elem, Index);
        return;
      end if;
    end loop;
    Bounded_Ordered_Collection_Nodes.Append (C.Rep.all, Elem);
  end Append;

  procedure Remove (C : in out Bounded_Ordered_Collection; At_Index : Positive) is
  begin
    Bounded_Ordered_Collection_Nodes.Remove (C.Rep.all, At_Index);
  end Remove;

  procedure Replace (C : in out Bounded_Ordered_Collection;
                     At_Index : Positive;
                     Elem : Item) is
  begin
    Bounded_Ordered_Collection_Nodes.Remove (C.Rep.all, At_Index);
    Insert (C, Elem);
  end Replace;

  function Available (C: in Bounded_Ordered_Collection) return Natural is
  begin
    return Bounded_Ordered_Collection_Nodes.Available (C.Rep.all);
  end Available;

  function Length (C : Bounded_Ordered_Collection) return Natural is
  begin
    return Bounded_Ordered_Collection_Nodes.Length (C.Rep.all);
  end Length;

  function Is_Empty (C : Bounded_Ordered_Collection) return Boolean is
  begin
    return Bounded_Ordered_Collection_Nodes.Length (C.Rep.all) = 0;
  end Is_Empty;

  function First (C : Bounded_Ordered_Collection) return Item is
  begin
    return Bounded_Ordered_Collection_Nodes.First (C.Rep.all);
  end First;

  function Last (C : Bounded_Ordered_Collection) return Item is
  begin
    return Bounded_Ordered_Collection_Nodes.Last (C.Rep.all);
  end Last;

  function Item_At
     (C : Bounded_Ordered_Collection; At_Index : Positive) return Item is
  begin
    return Item_At (C, At_Index).all;
  end Item_At;

  function Location
     (C : Bounded_Ordered_Collection; Elem : Item) return Natural is
  begin
    return Bounded_Ordered_Collection_Nodes.Location (C.Rep.all, Elem);
  end Location;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Bounded_Ordered_Collection);

  function New_Iterator
     (For_The_Collection : Bounded_Ordered_Collection) return Iterator'Class is
    Result : Collection_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Collection'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  function Item_At
     (C : Bounded_Ordered_Collection; Index : Positive) return Item_Ptr is
  begin
    return Bounded_Ordered_Collection_Nodes.Item_At (C.Rep.all, Index);
  end Item_At;

  procedure Adjust (C : in out Bounded_Ordered_Collection) is
  begin
    C.Rep := Bounded_Ordered_Collection_Nodes.Create (From => C.Rep.all);
  end Adjust;

  procedure Finalize (C : in out Bounded_Ordered_Collection) is
  begin
    Bounded_Ordered_Collection_Nodes.Free (C.Rep);
  end Finalize;

  Empty_Container : Bounded_Ordered_Collection;

  function Null_Container return Bounded_Ordered_Collection is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Collections.Ordered.Bounded;
