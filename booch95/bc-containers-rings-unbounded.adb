-- Copyright (C) 1994-1999 Grady Booch and Simon Wright.
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

package body BC.Containers.Rings.Unbounded is

  ---------
  -- "=" --
  ---------

  function "=" (Left, Right : in Unbounded_Ring) return Boolean is
    use Unbounded_Ring_Nodes;
  begin
    return Left.Top = Right.Top and then Left.Rep.all = Right.Rep.all;
  end "=";

  ---------
  -- Add --
  ---------

  procedure Add (R : in out Unbounded_Ring; Elem : Item) is
  begin
    Unbounded_Ring_Nodes.Append (R.Rep.all, Elem);
  end Add;

  ------------
  -- Adjust --
  ------------

  procedure Adjust (R : in out Unbounded_Ring) is
  begin
    R.Rep := Unbounded_Ring_Nodes.Create (From => R.Rep.all);
  end Adjust;

  -----------------
  -- Cardinality --
  -----------------

  function Cardinality (R : Unbounded_Ring) return Natural is
  begin
    return Unbounded_Ring_Nodes.Length (R.Rep.all);
  end Cardinality;

  -----------
  -- Clear --
  -----------

  procedure Clear (R : in out Unbounded_Ring) is
  begin
    Unbounded_Ring_Nodes.Clear (R.Rep.all);
    R.Top := 0;
    R.Mark := 0;
  end Clear;

  ------------
  -- Extent --
  ------------

  function Extent (R : Unbounded_Ring) return Natural is
  begin
    return Unbounded_Ring_Nodes.Length (R.Rep.all);
  end Extent;

  --------------
  -- Finalize --
  --------------

  procedure Finalize (R : in out Unbounded_Ring) is
  begin
    Unbounded_Ring_Nodes.Free (R.Rep);
  end Finalize;

  ----------------
  -- Initialize --
  ----------------

  procedure Initialize (R : in out Unbounded_Ring) is
  begin
    Initialize (Ring (R));
  end Initialize;

  ------------
  -- Insert --
  ------------

  procedure Insert (R : in out Unbounded_Ring; Elem : Item) is
  begin
    Unbounded_Ring_Nodes.Insert (R.Rep.all, Elem);
    if R.Top = 0 then
      R.Top := 1;
      R.Mark := 1;
    end if;
    if R.Mark > 1 then
      R.Mark := R.Mark + 1;
    end if;
  end Insert;

  --------------
  -- Is_Empty --
  --------------

  function Is_Empty (R : Unbounded_Ring) return Boolean is
  begin
    return Unbounded_Ring_Nodes.Length (R.Rep.all) = 0;
  end Is_Empty;

  -------------
  -- Item_At --
  -------------

  function Item_At (R : Unbounded_Ring; Index : Positive) return Item_Ptr is
  begin
    return Unbounded_Ring_Nodes.Item_At (R.Rep.all, Index);
  end Item_At;

  ------------------
  -- New_Iterator --
  ------------------

  package Address_Conversions
  is new System.Address_To_Access_Conversions (Unbounded_Ring);

  function New_Iterator (For_The_Ring : Unbounded_Ring) return Iterator is
    P : Address_Conversions.Object_Pointer
       := Address_Conversions.To_Pointer (For_The_Ring'Address);
  begin
    return Iterator (SP.Create (new Ring_Iterator (P)));
  end New_Iterator;

  ---------
  -- Pop --
  ---------

  procedure Pop (R : in out Unbounded_Ring) is
    Size : Natural;
  begin
    Unbounded_Ring_Nodes.Remove (R.Rep.all, R.Top);
    Size := Cardinality (R);
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

  -----------
  -- Purge --
  -----------

  procedure Purge (R : in out Unbounded_Ring) is
  begin
    Unbounded_Ring_Nodes.Clear (R.Rep.all);
    R.Top := 0;
    R.Mark := 0;
  end Purge;

  ------------
  -- Rotate --
  ------------

  procedure Rotate (R : in out Unbounded_Ring; Dir : Direction := Forward) is
  begin
    if Dir = Forward then
      R.Top := R.Top + 1;
      if R.Top > Cardinality (R) then
        R.Top := 1;
      end if;
    else
      if R.Top = 1 then
        R.Top := Cardinality (R);
      else
        R.Top := R.Top - 1;
      end if;
    end if;
  end Rotate;

  ---------
  -- Top --
  ---------

  function Top (R : Unbounded_Ring) return Item is
  begin
    return Unbounded_Ring_Nodes.Item_At (R.Rep.all, R.Top);
  end Top;

end BC.Containers.Rings.Unbounded;
