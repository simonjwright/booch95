-- Copyright (C) 1994-1999 Grady Booch, David Weller, Steve Doiel
-- and Simon Wright.
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

  procedure Reset (It : in out Iterator) is
  begin
    Reset (SP.Value (SP.Pointer (It)).all);
  end Reset;

  procedure Next (It : in out Iterator) is
  begin
    Next (SP.Value (SP.Pointer (It)).all);
  end Next;

  function Is_Done (It : Iterator) return Boolean is
  begin
    return Is_Done (SP.Value (SP.Pointer (It)).all);
  end Is_Done;

  function Current_Item (It : Iterator) return Item is
  begin
    return Current_Item (SP.Value (SP.Pointer (It)).all);
  end Current_Item;

  procedure Access_Current_Item is
  begin
    Apply (Current_Item (SP.Value (SP.Pointer (In_The_Iterator)).all).all);
  end Access_Current_Item;

  procedure Delete_Item_At (It : Iterator) is
  begin
    Delete_Item_At (SP.Value (SP.Pointer (It)).all);
  end Delete_Item_At;

  procedure Visit (Using : in out Iterator) is
    Success : Boolean;
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Apply (Current_Item (Using), Success);
      exit when not Success;
      Next (Using);
    end loop;
  end Visit;

  procedure Visit_With_In_Param (Using : in out Iterator;
                                 Param : Param_Type) is
    Success : Boolean;
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Apply (Current_Item (Using), Param, Success);
      exit when not Success;
      Next (Using);
    end loop;
  end Visit_With_In_Param;

  procedure Visit_With_In_Out_Param (Using : in out Iterator;
                                     Param : in out Param_Type) is
    Success : Boolean;
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Apply (Current_Item (Using), Param, Success);
      exit when not Success;
      Next (Using);
    end loop;
  end Visit_With_In_Out_Param;

  procedure Modify (Using : in out Iterator) is
    Success : Boolean;
    procedure Caller (I : in out Item) is
    begin
      Apply (I, Success);
    end Caller;
    procedure Call_Apply is new Access_Current_Item (Caller, Using);
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Call_Apply;
      exit when not Success;
      Next (Using);
    end loop;
  end Modify;

  procedure Modify_With_In_Param (Using : in out Iterator;
                                  Param : in Param_Type ) is
    Success : Boolean;
    procedure Caller (I : in out Item) is
    begin
      Apply (I, Param, Success);
    end Caller;
    procedure Call_Apply is new Access_Current_Item (Caller, Using);
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Call_Apply;
      exit when not Success;
      Next (Using);
    end loop;
  end Modify_With_In_Param;

  procedure Modify_With_In_Out_Param (Using : in out Iterator;
                                      Param : in out Param_Type) is
    Success : Boolean;
    procedure Caller (I : in out Item) is
    begin
      Apply (I, Param, Success);
    end Caller;
    procedure Call_Apply is new Access_Current_Item (Caller, Using);
  begin
    Reset (Using);
    while not Is_Done (Using) loop
      Call_Apply;
      exit when not Success;
      Next (Using);
    end loop;
  end Modify_With_In_Out_Param;

end BC.Containers;
