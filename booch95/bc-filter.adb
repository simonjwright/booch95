--  Copyright (C) 2001-2002 Simon Wright.
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

procedure BC.Filter (Input : From; Output : in out To) is
   It : Source.Iterator'Class := New_Iterator (Input);
begin
   Clear (Output);
   while not Source.Is_Done (It) loop
      declare
         I : constant Item := Source.Current_Item (It);
      begin
         if Pass (I) then
            Add (Output, I);
         end if;
      end;
      Source.Next (It);
   end loop;
end BC.Filter;
