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

package body BC.Containers is

  procedure Reset (Obj : in out Iterator) is
  begin
    Reset (SP.Value (SP.Pointer (Obj)).all);
  end Reset;

  procedure Next (Obj : in out Iterator) is
  begin
    Next (SP.Value (SP.Pointer (Obj)).all);
  end Next;

  function Is_Done (Obj : Iterator) return Boolean is
  begin
    return Is_Done (SP.Value (SP.Pointer (Obj)).all);
  end Is_Done;

  function Current_Item (Obj : Iterator) return Item is
  begin
    return Current_Item (SP.Value (SP.Pointer (Obj)).all);
  end Current_Item;

  procedure Visit is
    It : Iterator := New_Iterator (Over_The_Container);
    Success : Boolean;
  begin
    while not Is_Done (It) loop
      Apply (Current_Item (It), Success);
      exit when not Success;
      Next (It);
    end loop;
  end Visit;

  procedure Modify is
    It : Iterator := New_Iterator (Over_The_Container);
    Temp : Item;
    Success : Boolean;
  begin
    raise Program_Error;
    while not Is_Done (It) loop
      Temp := Current_Item (It);
      Apply (Temp, Success);
      exit when not Success;
      Next (It);
    end loop;
  end Modify;

end BC.Containers;
