-- Copyright (C) 2001 Simon Wright.
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

with Ada.Text_Io;
with BC.Containers.Quicksort;
with Collection_Test_Support;

procedure Sort_Test is

  use Ada.Text_Io;
  use Collection_Test_Support;
  use Containers;

  procedure Print (C : Container'Class) is
    procedure Process (C : Character; OK : out Boolean) is
    begin
      Put(C);
      Ok := True;
    end Process;
    procedure Iterate is new Visit (Apply => Process);
    Iter : Iterator'Class := New_Iterator (C);
  begin
    Iterate (Using => Iter);
    New_Line;
  end Print;

  procedure Sort is new Containers.Quicksort
     (Container => CU.Collection,
      Length => CU.Length);

  procedure Reverse_Sort is new Containers.Quicksort
     (Container => CU.Collection,
      "<" => ">",
      Length => CU.Length);

  C : CU.Collection;

  procedure Add (To : in out CU.Collection; S : String) is
  begin
    for Ch in S'Range loop
      CU.Append (To, S (Ch));
    end loop;
  end Add;

begin

  Add (C, "holy_moses");
  Sort (C);
  Print (C);
  Reverse_Sort (C);
  Print (C);

  New_Line;

  Add (C, "take_a_look");
  Sort (C);
  Print (C);
  Reverse_Sort (C);
  Print (C);

  New_Line;
  Put_Line ("equal keys, starting from empty");

  CU.Clear (C);
  Sort (C);
  Print (C);

  Add (C, "a");
  Sort (C);
  Print (C);

  Add (C, "a");
  Sort (C);
  Print (C);

  Add (C, "a");
  Sort (C);
  Print (C);

  Add (C, "a");
  Sort (C);
  Print (C);

  New_Line;
  Put_Line ("length 2");
  CU.Clear (C);
  Add (C, "b");
  Add (C, "a");
  Sort (C);
  Print (C);
  Reverse_Sort (C);
  Print (C);

end Sort_Test;
