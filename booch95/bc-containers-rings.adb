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

package body BC.Containers.Rings is

  function Are_Equal (Left, Right : Ring'Class) return Boolean is
  begin
    if System."=" (Left'Address, Right'Address) then
      return True;
    end if;
    if Extent (Left) /= Extent (Right) then
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

  procedure Copy (From : Ring'Class; To : in out Ring'Class) is
    Iter : Iterator'Class := New_Iterator (From);
  begin
    if System."/=" (From'Address, To'Address) then
      Clear (To);
      Reset (Iter);
      while not Is_Done (Iter) loop
        Add (To, Current_Item (Iter));
        Next (Iter);
      end loop;
      To.Top := From.Top;
      To.Mark := From.Mark;
      if Extent (To) > 0 then
        if To.Mark >= To.Top then                 -- XXX huh?
             To.Mark := To.Mark - To.Top;
        else
          To.Mark := To.Mark + To.Top + 1;
        end if;
      end if;
    end if;
  end Copy;

  procedure Mark (R : in out Ring) is
  begin
    R.Mark := R.Top;
  end Mark;

  procedure Rotate_To_Mark (R : in out Ring) is
  begin
    R.Top := R.Mark;
  end Rotate_To_Mark;

  function At_Mark (R : Ring) return Boolean is
  begin
    return R.Mark = R.Top;
  end At_Mark;

  procedure Initialize (R : in out Ring) is
  begin
    R.Top := 0;
    R.Mark := 0;
  end Initialize;

  procedure Add (R : in out Ring; Elem : Item) is
  begin
    raise Should_Have_Been_Overridden;
  end Add;

  procedure Reset (It : in out Ring_Iterator) is
    R : Ring'Class renames Ring'Class (It.For_The_Container.all);
  begin
    if Extent (R) = 0 then
      It.Index := 0;
    else
      It.Index := 1;
    end if;
  end Reset;

  function Is_Done (It : Ring_Iterator) return Boolean is
    R : Ring'Class renames Ring'Class (It.For_The_Container.all);
  begin
    return It.Index = 0 or else It.Index > Extent (R);
  end Is_Done;

  procedure Next (It : in out Ring_Iterator) is
  begin
    It.Index := It.Index + 1;
  end Next;

  function Current_Item (It : Ring_Iterator) return Item is
    R : Ring'Class renames Ring'Class (It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    declare
      Size : constant Positive := Extent (R);
      I : Positive;
    begin
      I := It.Index + R.Top - 1;
      if I > Size then
        I := I - Size;
      end if;
      return Item_At (R, I).all;
    end;
  end Current_Item;

  function Current_Item_Ptr (It : Ring_Iterator) return Item_Ptr is
    R : Ring'Class renames Ring'Class (It.For_The_Container.all);
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    declare
      Size : constant Positive := Extent (R);
      I : Positive;
    begin
      I := It.Index + R.Top - 1;
      if I > Size then
        I := I - Size;
      end if;
      return Item_At (R, I);
    end;
  end Current_Item_Ptr;

  procedure Delete_Item_At (It : Ring_Iterator) is
  begin
    if Is_Done (It) then
      raise BC.Not_Found;
    end if;
    raise BC.Not_Yet_Implemented;
  end Delete_Item_At;

end BC.Containers.Rings;
