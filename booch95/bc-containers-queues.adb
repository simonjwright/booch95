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

package body BC.Containers.Queues is

  function Are_Equal (Left, Right : Queue'Class) return Boolean is
  begin
    -- XXX left out the optimisation which checks whether L, R are
    -- identical.
    if Cardinality (Left) /= Cardinality (Right) then
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

  procedure Copy (From : Queue'Class; To : in out Queue'Class) is
    Iter : Iterator := New_Iterator (From);
  begin
    Clear (To);
    Reset (Iter);
    while not Is_Done (Iter) loop
      Append (To, Current_Item (Iter));
      Next (Iter);
    end loop;
  end Copy;

  procedure Initialize (It : in out Queue_Iterator) is
  begin
    Reset (It);
  end Initialize;

  procedure Reset (It : in out Queue_Iterator) is
  begin
    if Cardinality (It.Q.all) = 0 then
      It.Index := 0;
    else
      It.Index := 1;
    end if;
  end Reset;

  procedure Next (It : in out Queue_Iterator) is
  begin
    It.Index := It.Index + 1;
  end Next;

  function Is_Done (It : Queue_Iterator) return Boolean is
  begin
    return It.Index = 0 or else It.Index > Cardinality (It.Q.all);
  end Is_Done;

  function Current_Item (It : Queue_Iterator) return Item is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.Q.all, It.Index).all;
  end Current_Item;

  function Current_Item (It : Queue_Iterator) return Item_Ptr is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    return Item_At (It.Q.all, It.Index);
  end Current_Item;

end BC.Containers.Queues;
