-- Copyright (C) 1999 Simon Wright.
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

with Ada.Numerics.Discrete_Random;
with Ada.Text_Io;
with AVL_Test_Support;

procedure AVL_Test is

  use Ada.Text_IO;
  use AVL_Test_Support;
  use TA;

  package Rand is new Ada.Numerics.Discrete_Random (Item);
  use Rand;
  G : Generator;

  T : AVL_Tree;
  Loops : Natural := 0;

begin

  Put_Line ("Starting AVL tests");

  Reset (G);

  -- insert and remove random items until the tree is a half full
  while Extent (T) < Natural (Item'Last / 2) loop
    declare
      Dummy : Boolean;
    begin
      Insert (Obj => T,
              Element => Random (G),
              Not_Found => Dummy);
      Delete (Obj => T,
              Element => Random (G),
              Found => Dummy);
      Validate (T);
      Loops := Loops + 1;
    end;
  end loop;

  Put_Line ("that took" & Natural'Image (Loops) & " loops.");

  Put_Line ("Completed AVL tests");

end AVL_Test;
