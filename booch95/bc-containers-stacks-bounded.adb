-- Copyright (C) 1994-1998 Grady Booch, David Weller and Simon Wright.
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

package body BC.Containers.Stacks.Bounded is

  procedure Clear (Obj : in out Bnd_Stack) is
  begin
    Bnd_Stack_Nodes.Clear (Obj.Rep.all);
  end Clear;

  procedure Push (Obj : in out Bnd_Stack; Elem : Item) is
  begin
    Bnd_Stack_Nodes.Insert (Obj.Rep.all, Elem);
  end Push;

  procedure Pop (Obj : in out Bnd_Stack) is
  begin
    Bnd_Stack_Nodes.Remove (Obj.Rep.all, 1);
  end Pop;

  function Available (Obj: in Bnd_Stack) return Natural is
  begin
    return Bnd_Stack_Nodes.Available (Obj.Rep.all);
  end Available;

  function Depth (Obj : Bnd_Stack) return Natural is
  begin
    return Bnd_Stack_Nodes.Length (Obj.Rep.all);
  end Depth;

  function Is_Empty (Obj : Bnd_Stack) return Boolean is
  begin
    return Bnd_Stack_Nodes.Length (Obj.Rep.all) = 0;
  end Is_Empty;

  function Top (Obj : Bnd_Stack) return Item is
  begin
    return Bnd_Stack_Nodes.First (Obj.Rep.all);
  end Top;

  function Top (Obj : in Bnd_Stack) return Item_Ptr is
  begin
    return Bnd_Stack_Nodes.First (Obj.Rep.all'access);
  end Top;

  function "=" (Left, Right : Bnd_Stack) return Boolean is
  begin
    return Left.Rep.all = Right.Rep.all;
  end "=";

  function Cardinality (Obj : Bnd_Stack) return Integer is
  begin
    return Bnd_Stack_Nodes.Length (Obj.Rep.all);
  end Cardinality;

  procedure Purge (Obj : in out Bnd_Stack) is
  begin
    Bnd_Stack_Nodes.Clear (Obj.Rep.all);
  end Purge;

  procedure Add (Obj : in out Bnd_Stack; Elem : in out Item) is
  begin
    Bnd_Stack_Nodes.Append (Obj.Rep.all, Elem);
  end Add;

  function Item_At (Obj : in Bnd_Stack; Index : in Natural) return Item_Ptr is
  begin
    return Bnd_Stack_Nodes.Item_At (Obj.Rep, Index);
  end Item_at;

  procedure Adjust (Obj : in out Bnd_Stack) is
  begin
    Obj.Rep := Bnd_Stack_Nodes.Create (Obj.Rep.all);
  end Adjust;

  procedure Finalize (Obj : in out Bnd_Stack) is
  begin
    Free (Obj.Rep);
  end Finalize;

end BC.Containers.Stacks.Bounded;
