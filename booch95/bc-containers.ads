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

with Ada.Finalization;

generic
  type Item is private;
  type Item_Ptr is access all Item;
package BC.Containers is

  type Container is abstract new Ada.Finalization.Controlled with private;

  type Iterator (C : access Container'Class) is limited private;

  procedure Reset (Obj : in out Iterator);
  procedure Next (Obj : in out Iterator);
  function Is_Done (Obj : Iterator) return Boolean;
  function Current_Item (Obj : Iterator) return Item;
  function Current_Item (Obj : Iterator) return Item_Ptr;

  type Passive_Iterator (C : access Container'Class) is limited private;

  generic
    with procedure Apply (Elem : in Item; OK : out Boolean);
  function Visit (Obj : access Passive_Iterator) return Boolean;

  generic
    with procedure Apply (Elem_Ref : in Item_Ptr; OK : out Boolean);
  function Modify (Obj : access Passive_Iterator) return Boolean;

private

  type Container is abstract new Ada.Finalization.Controlled with null record;

  function Item_At (Obj : Container; Index : Natural) return Item_Ptr;
  function Cardinality (Obj : Container) return Integer;

  type Iterator (C : access Container'Class) is limited record
    Index : Integer := 1;
  end record;

  type Passive_Iterator (C : access Container'Class) is limited record
    Success : Boolean := False;
  end record;

end BC.Containers;

