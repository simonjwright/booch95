--  Copyright (C) 1994-2002 Grady Booch and Simon Wright.
--  All Rights Reserved.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with System;

package body BC.Containers.Maps is


   function Are_Equal (L, R : Abstract_Map'Class) return Boolean is
      It : Map_Iterator'Class := Map_Iterator'Class (New_Iterator (L));
   begin
      if System."=" (L'Address, R'Address) then
         return True;
      end if;
      if Extent (L) /= Extent (R) then
         return False;
      end if;
      while not Is_Done (It) loop
         if not Is_Bound (R, Current_Key (It))
           or else Item_Of (L, Current_Key (It))
                      /= Item_Of (R, Current_Key (It))
         then
            return False;
         end if;
         Next (It);
      end loop;
      return True;
   end Are_Equal;


   function Available (M : Abstract_Map) return Natural is
      pragma Warnings (Off, M);
   begin
      return Natural'Last;
   end Available;


   procedure Visit (Using : in out Map_Iterator'Class) is
      M : Abstract_Map'Class
        renames Abstract_Map'Class (Using.For_The_Container.all);
      Status : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Key (Using),
                Current_Item (Using),
                Status);
         exit when not Status;
         Next (Using);
      end loop;
   end Visit;


   procedure Modify (Using : in out Map_Iterator'Class) is
      M : Abstract_Map'Class
        renames Abstract_Map'Class (Using.For_The_Container.all);
      Status : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Key (Using),
                Current_Item_Ptr (Using).all,
                Status);
         exit when not Status;
         Next (Using);
      end loop;
   end Modify;


end BC.Containers.Maps;
