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

  function Cardinality (C : Container) return Natural is
  begin
    raise Should_Have_Been_Overridden;
    return 0;
  end Cardinality;

  procedure Purge (C : in out Container) is
  begin
    raise Should_Have_Been_Overridden;
  end Purge;

  function Item_At (C : Container; Index : Positive) return Item_Ptr is
  begin
    raise Should_Have_Been_Overridden;
    return null;
  end Item_At;

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

  procedure Access_Current_Item is
  begin
    Apply (Current_Item (SP.Value (SP.Pointer (In_The_Iterator)).all).all);
  end Access_Current_Item;

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
    Success : Boolean;
    procedure Caller (I : in out Item) is
    begin
      Apply (I, Success);
    end Caller;
    procedure Call_Apply is new Access_Current_Item (Caller, It);
  begin
    while not Is_Done (It) loop
      Call_Apply;
      exit when not Success;
      Next (It);
    end loop;
  end Modify;

end BC.Containers;
