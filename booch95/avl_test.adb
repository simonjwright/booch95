--  Copyright (C) 1999, 2001 Simon Wright.
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
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with AVL_Test_Support;

procedure AVL_Test is

   use Ada.Text_IO;
   use AVL_Test_Support;
   use TA;

   package Rand is new Ada.Numerics.Discrete_Random (Key);
   use Rand;
   G : Generator;

   T : AVL_Tree;
   Loops : Natural := 0;

   Inserted : Boolean;

begin

   Put_Line ("Starting AVL insertion/replacement tests");

   Insert (T => T,
           Element => (K => 497, Count => 42),
           Not_Found => Inserted);
   Insert (T => T,
           Element => (K => 42, Count => 497),
           Not_Found => Inserted);
   Print (T);
   Insert (T => T,
           Element => (K => 497, Count => 321),
           Not_Found => Inserted);
   Insert (T => T,
           Element => (K => 42, Count => 123),
           Not_Found => Inserted);
   Validate (T);
   Print (T);

   Put_Line ("Starting AVL insertion/deletion tests");

   Reset (G);

   --  insert and remove random items until the tree is half full
   while Extent (T) < Natural (Key'Last / 2) loop
      declare
         Dummy : Boolean;
      begin
         Insert (T => T,
                 Element => (K => Random (G), Count => 1234),
                 Not_Found => Dummy);
         Delete (T => T,
                 Element => (K => Random (G), Count => 2345),
                 Found => Dummy);
         Validate (T);
         Loops := Loops + 1;
      end;
   end loop;

   Put_Line ("that took" & Natural'Image (Loops) & " loops.");

   Put_Line ("Completed AVL tests");

exception
   when E : others =>
      Put_Line ("                                   EXCEPTION "
                & Ada.Exceptions.Exception_Name (E)
                & " OCCURRED.");
end AVL_Test;
