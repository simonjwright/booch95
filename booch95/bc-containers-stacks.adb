-- Copyright (C) 1994-2000 Grady Booch, David Weller and Simon Wright.
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

with System;

package body BC.Containers.Stacks is

  procedure Process_Top (S : in out Stack'Class) is
  begin
    Process (Item_At (S, 1).all);
  end Process_Top;

  function Are_Equal (Left, Right : Stack'Class) return Boolean is
  begin
    if System."=" (Left'Address, Right'Address) then
      return True;
    end if;
    if Depth (Left) /= Depth (Right) then
      return False;
    end if;
    declare
      Left_Iter : Iterator'Class := New_Iterator (Left);
      Right_Iter : Iterator'Class := New_Iterator (Right);
    begin
      while not Is_Done (Left_Iter) and then
         not Is_Done (Right_Iter) loop
        if Current_Item (Left_Iter) /= Current_Item (Right_Iter) then
          return False;
        end if;
        Next (Left_Iter);
        Next (Right_Iter);
      end loop;
      return True;
    end;
  end Are_Equal;

  procedure Copy (From : Stack'Class; To : in out Stack'Class) is
    Iter : Iterator'Class := New_Iterator (From);
  begin
    if System."/=" (From'Address, To'Address) then
      Clear (To);
      Reset (Iter);
      while not Is_Done (Iter) loop
        Add (To, Current_Item (Iter));
        Next (Iter);
      end loop;
    end if;
  end Copy;

  -- Subprograms to be overridden

  procedure Add (S : in out Stack; Elem : Item) is
  begin
    raise Should_Have_Been_Overridden;
  end Add;

  procedure Remove (S : in out Stack; From : Positive) is
  begin
    raise Should_Have_Been_Overridden;
  end Remove;

  -- Iterators

  procedure Reset (It : in out Stack_Iterator) is
    S : Stack'Class renames Stack'Class (It.For_The_Container.all);
  begin
    if Depth (S) = 0 then
      It.Index := 0;
    else
      It.Index := 1;
    end if;
  end Reset;

  procedure Next (It : in out Stack_Iterator) is
  begin
    It.Index := It.Index + 1;
  end Next;

  function Is_Done (It : Stack_Iterator) return Boolean is
    S : Stack'Class renames Stack'Class (It.For_The_Container.all);
  begin
    return It.Index = 0 or else It.Index > Depth (S);
  end Is_Done;

  function Current_Item (It : Stack_Iterator) return Item is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.For_The_Container.all, It.Index).all;
  end Current_Item;

  function Current_Item_Ptr (It : Stack_Iterator) return Item_Ptr is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.For_The_Container.all, It.Index);
  end Current_Item_Ptr;

  procedure Delete_Item_At (It : Stack_Iterator) is
    S : Stack'Class renames Stack'Class (It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    Remove (S, It.Index);
  end Delete_Item_At;

end BC.Containers.Stacks;
