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

with System;

package body BC.Containers.Collections is

  function Are_Equal (Left, Right : Abstract_Collection'Class) return Boolean is
  begin
    if System."=" (Left'Address, Right'Address) then
      return True;
    end if;
    if Length (Left) /= Length (Right) then
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

  procedure Copy (From : Abstract_Collection'Class;
                  To : in out Abstract_Collection'Class) is
    Iter : Iterator'Class := New_Iterator (From);
  begin
    if System."/=" (From'Address, To'Address) then
      Clear (To);
      Reset (Iter);
      while not Is_Done (Iter) loop
        -- doing Append will preserve ordering of equal-key items in
        -- Ordered Collections.
        Append (To, Current_Item (Iter));
        Next (Iter);
      end loop;
   end if;
  end Copy;

  procedure Lock (C : in out Abstract_Collection) is
  begin
    null;
  end Lock;

  procedure Unlock (C : in out Abstract_Collection) is
  begin
    null;
  end Unlock;

  procedure Reset (It : in out Collection_Iterator) is
    C : Abstract_Collection'Class
       renames Abstract_Collection'Class(It.For_The_Container.all);
  begin
    if Length (C) = 0 then
      It.Index := 0;
    else
      It.Index := 1;
    end if;
  end Reset;

  function Is_Done (It : Collection_Iterator) return Boolean is
    C : Abstract_Collection'Class
    renames Abstract_Collection'Class(It.For_The_Container.all);
  begin
    return It.Index = 0 or else It.Index > Length (C);
  end Is_Done;

  procedure Next (It : in out Collection_Iterator) is
  begin
    It.Index := It.Index + 1;
  end Next;

  function Current_Item (It : Collection_Iterator) return Item is
    C : Abstract_Collection'Class
    renames Abstract_Collection'Class(It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (C, It.Index).all;
  end Current_Item;

  function Current_Item_Ptr (It : Collection_Iterator) return Item_Ptr is
    C : Abstract_Collection'Class
    renames Abstract_Collection'Class(It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (C, It.Index);
  end Current_Item_Ptr;

  procedure Delete_Item_At (It : in out Collection_Iterator) is
    C : Abstract_Collection'Class
       renames Abstract_Collection'Class(It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    Remove (C, It.Index);
  end Delete_Item_At;

end BC.Containers.Collections;
