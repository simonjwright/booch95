--  Copyright (C) 1999,2001 Simon Wright.
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

with Ada.Calendar;
with Ada.Text_IO;
with BC.Support.Synchronization;

procedure Test_Synchronization is

   use Ada.Text_IO;
   use BC.Support.Synchronization;

   task type Semaphore_Test (Id : Positive; Using : access Semaphore) is
      entry Start;
      entry Stop;
   end Semaphore_Test;

   task body Semaphore_Test is
   begin
      accept Start;
      loop
         Put_Line ("semaphore_test" & Positive'Image (Id) & " seizing ..");
         Seize (Using.all);
         Put_Line ("semaphore_test" & Positive'Image (Id) & " seized.");
         delay 2.0;
         Put_Line ("semaphore_test" & Positive'Image (Id) & " releasing.");
         Release (Using.all);
         select
            accept Stop;
            Put_Line ("semaphore_test" & Positive'Image (Id) & " stopping.");
            exit;
         else
            delay 1.0;
         end select;
      end loop;
   end Semaphore_Test;

   task type Reader (Id : Positive; Using : access Monitor_Base'Class) is
      entry Start;
      entry Stop;
   end Reader;

   task body Reader is
   begin
      accept Start;
      loop
         Put_Line ("reader" & Positive'Image (Id) & " starting to read ..");
         declare
            L : Read_Lock (Using);
            pragma Warnings (Off, L);
         begin
            Put_Line ("reader" & Positive'Image (Id) & " reading ..");
            delay 1.0;
            Put_Line ("reader" & Positive'Image (Id) & " done.");
         end;
         select
            accept Stop;
            Put_Line ("reader" & Positive'Image (Id) & " stopping.");
            exit;
         else
            delay 1.0;
         end select;
      end loop;
   end Reader;

   task type Writer (Id : Positive; Using : access Monitor_Base'Class) is
      entry Start;
      entry Stop;
   end Writer;

   task body Writer is
   begin
      accept Start;
      loop
         Put_Line ("writer" & Positive'Image (Id) & " starting to write ..");
         declare
            L : Write_Lock (Using);
            pragma Warnings (Off, L);
         begin
            Put_Line ("writer" & Positive'Image (Id) & " writing ..");
            delay 1.0;
            Put_Line ("writer" & Positive'Image (Id) & " done.");
         end;
         select
            accept Stop;
            Put_Line ("writer" & Positive'Image (Id) & " stopping.");
            exit;
         else
            delay 1.0;
         end select;
      end loop;
   end Writer;

begin

   Put_Line ("Test 1: Semaphore.");

   declare

      S : aliased Semaphore;

      St1 : Semaphore_Test (1, S'Access);
      St2 : Semaphore_Test (2, S'Access);

   begin
      St1.Start;
      delay 1.0;
      St2.Start;
      delay 10.0;
      Put_Line ("Test 1 stopping ..");
      St1.Stop;
      St2.Stop;
   end;

   Put_Line ("Test 1 stopped.");
   New_Line;

   Put_Line ("Test 2: Single Monitor.");

   declare

      M : aliased Single_Monitor;

      R1 : Reader (1, M'Access);
      R2 : Reader (2, M'Access);
      R3 : Reader (3, M'Access);
      W1 : Writer (1, M'Access);
      W2 : Writer (2, M'Access);
      W3 : Writer (3, M'Access);

   begin
      R1.Start;
      R2.Start;
      R3.Start;
      delay 1.0;
      W1.Start;
      W2.Start;
      W3.Start;
      delay 10.0;
      Put_Line ("Test 2 stopping ..");
      R1.Stop;
      R2.Stop;
      R3.Stop;
      W1.Stop;
      W2.Stop;
      W3.Stop;
   end;

   Put_Line ("Test 2 stopped.");
   New_Line;

   Put_Line ("Test 3: Multiple Monitor.");

   declare

      M : aliased Multiple_Monitor;

      R1 : Reader (1, M'Access);
      R2 : Reader (2, M'Access);
      R3 : Reader (3, M'Access);
      W1 : Writer (1, M'Access);
      W2 : Writer (2, M'Access);
      W3 : Writer (3, M'Access);

   begin
      R1.Start;
      R2.Start;
      R3.Start;
      delay 1.0;
      W1.Start;
      W2.Start;
      W3.Start;
      delay 10.0;
      Put_Line ("Test 3 stopping ..");
      R1.Stop;
      R2.Stop;
      R3.Stop;
      W1.Stop;
      W2.Stop;
      W3.Stop;
   end;

   Put_Line ("Test 3 stopped.");
   New_Line;

   Put_Line ("Test 4: Timing.");

   declare

      N : constant := 10_000;
      S : aliased Single_Monitor;
      Start : Ada.Calendar.Time;
      Null_Interval, Real_Interval : Duration;
      J : Integer := 0;
      use type Ada.Calendar.Time;

   begin

      Start := Ada.Calendar.Clock;
      for I in 1 .. N loop
         begin
            J := J + 1;
         end;
      end loop;
      Null_Interval := Ada.Calendar.Clock - Start;

      Start := Ada.Calendar.Clock;
      for I in 1 .. N loop
         declare
            L : Read_Lock (S'Access);
            pragma Warnings (Off, L);
         begin
            J := J + 1;
         end;
      end loop;
      Real_Interval := Ada.Calendar.Clock - Start;

      Ada.Text_IO.Put_Line
        ("Acquiring a read lock took"
         & Duration'Image ((Real_Interval - Null_Interval)/ N)
         & " seconds.");

   end;

   Put_Line ("Test 4 stopped.");
   New_Line;

end Test_Synchronization;
