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

with Ada.Text_IO;
with BC.Containers.Shellsort;
with BC.Containers.Quicksort;
with Collection_Test_Support;

procedure Sort_Test is

   use Ada.Text_IO;
   use Collection_Test_Support;
   use Containers;

   procedure Add (To : in out CB.Collection; S : String);
   procedure Print (C : Container'Class);

   procedure Print (C : Container'Class) is
      procedure Process (C : Character; OK : out Boolean);
      procedure Process (C : Character; OK : out Boolean) is
      begin
         Put (C);
         OK := True;
      end Process;
      procedure Iterate is new Visit (Apply => Process);
      Iter : Iterator'Class := New_Iterator (C);
   begin
      Put ("|");
      Iterate (Using => Iter);
      Put_Line ("|");
   end Print;

   procedure SSort is new Containers.Shellsort
     (Container => CB.Collection,
      Length => CB.Length);

   procedure Reverse_SSort is new Containers.Shellsort
     (Container => CB.Collection,
        "<" => ">",
      Length => CB.Length);

   procedure QSort is new Containers.Quicksort
     (Container => CB.Collection,
      Length => CB.Length);

   procedure Reverse_QSort is new Containers.Quicksort
     (Container => CB.Collection,
        "<" => ">",
      Length => CB.Length);

   C : CB.Collection;

   procedure Add (To : in out CB.Collection; S : String) is
   begin
      for Ch in S'Range loop
         CB.Append (To, S (Ch));
      end loop;
   end Add;

begin

   Put_Line ("Shellsort:");

   CB.Clear (C);

   Add (C, "holy_moses");
   SSort (C);
   Print (C);
   Reverse_SSort (C);
   Print (C);

   New_Line;

   Add (C, "take_a_look");
   SSort (C);
   Print (C);
   Reverse_SSort (C);
   Print (C);

   New_Line;
   Put_Line ("equal keys, starting from empty");

   CB.Clear (C);
   SSort (C);
   Print (C);

   Add (C, "a");
   SSort (C);
   Print (C);

   Add (C, "a");
   SSort (C);
   Print (C);

   Add (C, "a");
   SSort (C);
   Print (C);

   Add (C, "a");
   SSort (C);
   Print (C);

   New_Line;
   Put_Line ("length 2");
   CB.Clear (C);
   Add (C, "b");
   Add (C, "a");
   SSort (C);
   Print (C);
   Reverse_SSort (C);
   Print (C);

   New_Line;

   Put_Line ("Quicksort:");

   CB.Clear (C);

   Add (C, "holy_moses");
   QSort (C);
   Print (C);
   Reverse_QSort (C);
   Print (C);

   New_Line;

   Add (C, "take_a_look");
   QSort (C);
   Print (C);
   Reverse_QSort (C);
   Print (C);

   New_Line;
   Put_Line ("equal keys, starting from empty");

   CB.Clear (C);
   QSort (C);
   Print (C);

   Add (C, "a");
   QSort (C);
   Print (C);

   Add (C, "a");
   QSort (C);
   Print (C);

   Add (C, "a");
   QSort (C);
   Print (C);

   Add (C, "a");
   QSort (C);
   Print (C);

   New_Line;
   Put_Line ("length 2");
   CB.Clear (C);
   Add (C, "b");
   Add (C, "a");
   QSort (C);
   Print (C);
   Reverse_QSort (C);
   Print (C);

end Sort_Test;
