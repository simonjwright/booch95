-- Copyright (C) 2000 Simon Wright.
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

with Ada.Text_Io; use Ada.Text_Io;
with System.Storage_Elements;

package body BC.Support.Synchronization.Debug is


  function Image (The_Address : System.Address) return String is
    use System.Storage_Elements;
  begin
    return Integer_Address'Image (To_Integer (The_Address));
  end Image;


  procedure Initialize (The_Semaphore : in out Debug_Semaphore) is
  begin
    Put_Line ("Semaphore.Initialize:"
              & Image (The_Semaphore'Address));
    Initialize (Semaphore (The_Semaphore));
  end Initialize;

  procedure Adjust (The_Semaphore : in out Debug_Semaphore) is
  begin
    Put_Line ("Semaphore.Adjust:"
              & Image (The_Semaphore'Address));
    Adjust (Semaphore (The_Semaphore));
  end Adjust;

  procedure Finalize (The_Semaphore : in out Debug_Semaphore) is
  begin
    Put_Line ("Semaphore.Finalize:"
              & Image (The_Semaphore'Address));
    Finalize (Semaphore (The_Semaphore));
  end Finalize;

  procedure Seize (The_Semaphore : in out Debug_Semaphore) is
  begin
    Put_Line ("Semaphore.Seize:"
              & Image (The_Semaphore'Address));
    Seize (Semaphore (The_Semaphore));
  end Seize;

  procedure Release (The_Semaphore : in out Debug_Semaphore) is
  begin
    Put_Line ("Semaphore.Release:"
              & Image (The_Semaphore'Address));
    Release (Semaphore (The_Semaphore));
  end Release;


  procedure Initialize (The_Semaphore : in out Debug_Recursive_Semaphore) is
  begin
    Put_Line ("Recursive_Semaphore.Initialize:"
              & Image (The_Semaphore'Address));
    Initialize (Recursive_Semaphore (The_Semaphore));
  end Initialize;

  procedure Adjust (The_Semaphore : in out Debug_Recursive_Semaphore) is
  begin
    Put_Line ("Recursive_Semaphore.Adjust:"
              & Image (The_Semaphore'Address));
    Adjust (Recursive_Semaphore (The_Semaphore));
  end Adjust;

  procedure Finalize (The_Semaphore : in out Debug_Recursive_Semaphore) is
  begin
    Put_Line ("Recursive_Semaphore.Finalize:"
              & Image (The_Semaphore'Address));
    Finalize (Recursive_Semaphore (The_Semaphore));
  end Finalize;

  procedure Seize (The_Semaphore : in out Debug_Recursive_Semaphore) is
  begin
    Put_Line ("Recursive_Semaphore.Seize:"
              & Image (The_Semaphore'Address));
    Seize (Recursive_Semaphore (The_Semaphore));
  end Seize;

  procedure Release (The_Semaphore : in out Debug_Recursive_Semaphore) is
  begin
    Put_Line ("Recursive_Semaphore.Release:"
              & Image (The_Semaphore'Address));
    Release (Recursive_Semaphore (The_Semaphore));
  end Release;


  procedure Seize_For_Reading (The_Monitor : in out Debug_Single_Monitor) is
  begin
    Put_Line ("Single_Monitor.Seize_For_Reading:"
              & Image (The_Monitor'Address));
    Seize_For_Reading (Single_Monitor (The_Monitor));
  end Seize_For_Reading;

  procedure Seize_For_Writing (The_Monitor : in out Debug_Single_Monitor) is
  begin
    Put_Line ("Single_Monitor.Seize_For_Writing:"
              & Image (The_Monitor'Address));
    Seize_For_Writing (Single_Monitor (The_Monitor));
  end Seize_For_Writing;

  procedure Release_From_Reading (The_Monitor : in out Debug_Single_Monitor) is
  begin
    Put_Line ("Single_Monitor.Release_From_Reading:"
              & Image (The_Monitor'Address));
    Release_From_Reading (Single_Monitor (The_Monitor));
  end Release_From_Reading;

  procedure Release_From_Writing (The_Monitor : in out Debug_Single_Monitor) is
  begin
    Put_Line ("Single_Monitor.Release_From_Writing:"
              & Image (The_Monitor'Address));
    Release_From_Writing (Single_Monitor (The_Monitor));
  end Release_From_Writing;


  procedure Seize_For_Reading (The_Monitor : in out Debug_Multiple_Monitor) is
  begin
    Put_Line ("Multiple_Monitor.Seize_For_Reading:"
              & Image (The_Monitor'Address));
    Seize_For_Reading (Multiple_Monitor (The_Monitor));
  end Seize_For_Reading;

  procedure Seize_For_Writing (The_Monitor : in out Debug_Multiple_Monitor) is
  begin
    Put_Line ("Multiple_Monitor.Seize_For_Writing:"
              & Image (The_Monitor'Address));
    Seize_For_Writing (Multiple_Monitor (The_Monitor));
  end Seize_For_Writing;

  procedure Release_From_Reading (The_Monitor : in out Debug_Multiple_Monitor) is
  begin
    Put_Line ("Multiple_Monitor.Release_From_Reading:"
              & Image (The_Monitor'Address));
    Release_From_Reading (Multiple_Monitor (The_Monitor));
  end Release_From_Reading;

  procedure Release_From_Writing (The_Monitor : in out Debug_Multiple_Monitor) is
  begin
    Put_Line ("Multiple_Monitor.Release_From_Writing:"
              & Image (The_Monitor'Address));
    Release_From_Writing (Multiple_Monitor (The_Monitor));
  end Release_From_Writing;


end BC.Support.Synchronization.Debug;
