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

package body BC.Containers.Deques is

  procedure Process_Front (D : in out Deque'Class) is
  begin
    Process (Item_At (D, 1).all);
  end Process_Front;

  procedure Process_Back (D : in out Deque'Class) is
  begin
    Process (Item_At (D, Length (D)).all);
  end Process_Back;

  procedure Copy (From : Deque'Class; To : in out Deque'Class) is
    Iter : Iterator := New_Iterator (From);
  begin
    if System."/=" (From'Address, To'Address) then
      Clear (To);
      Reset (Iter);
      while not Is_Done (Iter) loop
        Append (To, Current_Item (Iter));
        Next (Iter);
      end loop;
    end if;
  end Copy;

  function Are_Equal (Left, Right : Deque'Class) return Boolean is
  begin
    if System."=" (Left'Address, Right'Address) then
      return True;
    end if;
    if Length (Left) /= Length (Right) then
      return False;
    end if;
    declare
      Left_Iter : Iterator := New_Iterator (Left);
      Right_Iter : Iterator := New_Iterator (Right);
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

  procedure Initialize (It : in out Deque_Iterator) is
  begin
    Reset (It);
  end Initialize;

  procedure Reset (It : in out Deque_Iterator) is
  begin
    if Length (It.D.all) = 0 then
      It.Index := 0;
    else
      It.Index := 1;
    end if;
  end Reset;

  procedure Next (It : in out Deque_Iterator) is
  begin
    It.Index := It.Index + 1;
  end Next;

  function Is_Done (It : Deque_Iterator) return Boolean is
  begin
    return It.Index = 0 or else It.Index > Length (It.D.all);
  end Is_Done;

  function Current_Item (It : Deque_Iterator) return Item is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.D.all, It.Index).all;
  end Current_Item;

  function Current_Item (It : Deque_Iterator) return Item_Ptr is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.D.all, It.Index);
  end Current_Item;

  procedure Delete_Item_At (It : Deque_Iterator) is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    Remove (It.D.all, It.Index);
  end Delete_Item_At;

end BC.Containers.Deques;
