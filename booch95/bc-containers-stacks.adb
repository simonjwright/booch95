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

package body Bc.Containers.Stacks is

  function "=" (Left, Right : access Stack'Class) return Boolean is
  begin
    if Left.all = Right.all then
      return True;
    elsif Cardinality (Left.all) /= Cardinality (Right.all) then
      return False;
    else
      declare
        Left_Iter : Iterator (Left);
        Right_Iter : Iterator (Right);
      begin
        while not Is_Done (Left_Iter) and then
           not Is_Done (Right_Iter) loop
          if Current_Item (Left_Iter).all /=
             Current_Item (Right_Iter).all then
            return False;
          end if;
          Next (Left_Iter);
          Next (Right_Iter);
        end loop;
        return True;
      end;
    end if;
  end "=";

  procedure Copy (From : access Stack'Class; To : access Stack'Class) is
    Iter : Iterator (From);
  begin
    Clear (To.all);
    Reset (Iter);
    while not Is_Done (Iter) loop
      Add (To.all, Current_Item (Iter).all);
      Next (Iter);
    end loop;
  end Copy;

  procedure Purge (Obj : in out Stack) is
  begin
    raise Should_Have_Been_Overridden;
  end Purge;

  procedure Add (Obj : in out Stack; Elem : in out Item) is
  begin
    raise Should_Have_Been_Overridden;
  end Add;

end Bc.Containers.Stacks;
