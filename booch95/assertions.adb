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

with Ada.Text_IO; use Ada.Text_IO;

package body Assertions is

   Failures : Natural := 0;

   procedure Reset is
   begin
      Failures := 0;
   end Reset;

   procedure Assertion (B : Boolean; S : String) is
   begin
      if not B then
         Failures := Failures + 1;
         Put_Line (S);
      end if;
   end Assertion;

   procedure Report is
   begin
      case Failures is
         when 0 =>
            Put_Line ("No failures.");
         when 1 =>
            Put_Line ("One failure.");
         when others =>
            Put_Line (Integer'Image (Failures) & " failures.");
      end case;
   end Report;

end Assertions;
