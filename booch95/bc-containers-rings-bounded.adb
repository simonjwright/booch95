-- Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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

package body BC.Containers.Rings.Bounded is

  function "=" (Left, Right : in Ring) return Boolean is
    use Ring_Nodes;
  begin
    return Left.Top = Right.Top and then Left.Rep.all = Right.Rep.all;
  end "=";

  procedure Clear (R : in out Ring) is
  begin
    Ring_Nodes.Clear (R.Rep.all);
    R.Top := 0;
    R.Mark := 0;
  end Clear;

  procedure Insert (R : in out Ring; Elem : Item) is
  begin
    if R.Top = 0 then
      R.Top := 1;
      R.Mark := 1;
      Ring_Nodes.Insert (R.Rep.all, Elem);
    else
      if R.Mark /= 0 then
        R.Mark := R.Mark + 1;
      end if;
      Ring_Nodes.Insert (R.Rep.all, Elem, Before => R.Top);
    end if;
  end Insert;

  procedure Pop (R : in out Ring) is
    Size : Natural;
  begin
    Ring_Nodes.Remove (R.Rep.all, R.Top);
    Size := Extent (R);
    if Size = 0 then
      R.Top := 0;
      R.Mark := 0;
    else
      if R.Mark > R.Top then
        R.Mark := R.Mark - 1;
      elsif R.Mark = R.Top and then R.Mark > Size then
        R.Mark := 1;
      end if;
      if R.Top > Size then
        R.Top := 1;
      end if;
    end if;
  end Pop;

  procedure Rotate (R : in out Ring; Dir : Direction := Forward) is
  begin
    if Dir = Forward then
      R.Top := R.Top + 1;
      if R.Top > Extent (R) then
        R.Top := 1;
      end if;
    else
      if R.Top = 1 then
        R.Top := Extent (R);
      else
        R.Top := R.Top - 1;
      end if;
    end if;
  end Rotate;

  function Available (R : in Ring) return Natural is
  begin
    return Ring_Nodes.Available (R.Rep.all);
  end Available;

  function Extent (R : Ring) return Natural is
  begin
    return Ring_Nodes.Length (R.Rep.all);
  end Extent;

  function Is_Empty (R : Ring) return Boolean is
  begin
    return Ring_Nodes.Length (R.Rep.all) = 0;
  end Is_Empty;

  function Top (R : Ring) return Item is
  begin
    return Ring_Nodes.Item_At (R.Rep.all, R.Top);
  end Top;

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Ring);

  function New_Iterator (For_The_Ring : Ring) return Iterator'Class is
    Result : Ring_Iterator;
  begin
    Result.For_The_Container :=
       Address_Conversions.To_Pointer (For_The_Ring'Address).all'Access;
    Reset (Result);
    return Result;
  end New_Iterator;

  procedure Add (R : in out Ring; Elem : Item) is
  begin
    Ring_Nodes.Append (R.Rep.all, Elem);
  end Add;

  function Item_At (R : Ring; Index : Positive) return Item_Ptr is
  begin
    return Ring_Nodes.Item_At (R.Rep.all, Index);
  end Item_At;

  procedure Initialize (R : in out Ring) is
  begin
    Initialize (Abstract_Ring (R));
  end Initialize;

  procedure Adjust (R : in out Ring) is
  begin
    R.Rep := Ring_Nodes.Create (From => R.Rep.all);
  end Adjust;

  procedure Finalize (R : in out Ring) is
  begin
    Ring_Nodes.Free (R.Rep);
  end Finalize;

  Empty_Container : Ring;

  function Null_Container return Ring is
  begin
    return Empty_Container;
  end Null_Container;

end BC.Containers.Rings.Bounded;
