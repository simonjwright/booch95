--  Copyright (C) 2001 Simon Wright.
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

--  $Id$

procedure BC.Filter (Input : From; Output : in out To) is
   procedure Ins (I : Item; OK : out Boolean);
   pragma Inline (Ins);
   procedure Filt is new Source.Visit (Ins);
   procedure Ins (I : Item; OK : out Boolean) is
   begin
      if Pass (I) then
         Add (Output, I);
      end if;
      OK := True;
   end Ins;
   It : Source.Iterator'Class := New_Iterator (Input);
begin
   Clear (Output);
   Filt (It);
end BC.Filter;
