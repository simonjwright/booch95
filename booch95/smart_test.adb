--  Copyright (C) 1998, 2001 Simon Wright.
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

with Ada.Exceptions;
with Ada.Text_IO;
with Smart_Test_Support;

procedure Smart_Test is
   use Ada.Text_IO;
   use Smart_Test_Support;
   A : Smart.Pointer;
   B : Smart.Pointer := A;
   C : Smart.Pointer;
begin

   Put_Line ("creating m, assigning to a");
   A := Create ('m');
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("assigning a to a");
   A := A;
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("assigning b to b");
   B := B;
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("assigning a to c");
   C := A;
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("assigning a to b");
   B := A;
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("assigning a to a");
   A := A;
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("creating n, assigning to a");
   A := Create ('n');
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("creating o, assigning to a");
   A := Create ('o');
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("assigning a to b");
   B := A;
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("creating p, assigning to c");
   C := Create ('p');
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("creating q, assigning to b");
   B := Create ('q');
   Put_Line ("a " & Value (A) & ", b " & Value (B) & ", c " & Value (C));

   Put_Line ("done.");

exception
   when E : others =>
      Put_Line ("                                   EXCEPTION "
                & Ada.Exceptions.Exception_Name (E)
                & " OCCURRED.");
end Smart_Test;
