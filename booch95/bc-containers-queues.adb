-- The Ada 95 Booch Components (Version 1.0 beta 1)
-- Copyright (C)1994-1997 Grady Booch and David Weller.  All Rights Reserved.
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
--  This file contains the implementation of the queue abstract base class
--  (The non-abstract parts, that is)

package body Bc.Containers.Queues is

   procedure Purge (Obj : in out Queue) is
   begin
      pragma Assert ( False, "Failure to override Purge operation");
      raise Abstraction_Violation;
      null;
   end Purge;

   procedure Add (Obj : in out Queue; Elem : in out Item) is
   begin
      pragma Assert ( False, "Failure to override Add operation");
      raise Abstraction_Violation;
      null;
   end Add;

   function "=" (Left, Right : access Queue'Class) return Boolean is
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

   procedure Copy (From : access Queue'Class; To : access Queue'Class) is
      Iter : Iterator (From);
   begin
      Clear (To.all);
      Reset (Iter);
      while not Is_Done (Iter) loop
	 Append (To.all, Current_Item (Iter).all);
	 Next (Iter);
      end loop;
   end Copy;

end Bc.Containers.Queues;


