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

with Ada.Text_Io;
with BC.Support.Synchronization;

procedure Test_Synchronization is

  use Ada.Text_Io;
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
      Using.Seize;
      Put_Line ("semaphore_test" & Positive'Image (Id) & " seized.");
      delay 2.0;
      Put_Line ("semaphore_test" & Positive'Image (Id) & " releasing.");
      Using.Release;
      select
        accept Stop;
        Put_Line ("semaphore_test" & Positive'Image (Id) & " stopping.");
        exit;
      else
        delay 1.0;
      end select;
    end loop;
  end Semaphore_Test;

  task type Reader (Id : Positive; Using : access Monitor'Class) is
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

  task type Writer (Id : Positive; Using : access Monitor'Class) is
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

  Put_Line ("Test 1: simple Semaphore, initial count.");

  declare

    S : aliased Semaphore (Initially_Seized => True);

    St1 : Semaphore_Test (1, S'Access);
    St2 : Semaphore_Test (2, S'Access);

  begin
    St1.Start;
    delay 1.0;
    St2.Start;
    delay 1.0;
    Put_Line ("Test 1 main loop releasing ..");
    S.Release;
    Put_Line ("Test 1 main loop released.");
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

end Test_Synchronization;
