-- Copyright (C) 1994-1999 Grady Booch and Simon Wright.
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

with Ada.Finalization;
with Ada.Task_Identification;

package BC.Support.Synchronization is


  -- A Semaphore is like a POSIX recursive mutex; once Seized by a
  -- task, that task can Seize again; other tasks are blocked until
  -- the owning task has Released the semaphore as many times as it
  -- Seized it.
  protected type Semaphore is
    entry Seize;
    procedure Release;
    function None_Pending return Boolean;
  private
    entry Waiting;
    Owner : Ada.Task_Identification.Task_Id;
    Count : Natural := 0;
  end Semaphore;

  type Semaphore_P is access all Semaphore;
  procedure Delete (The_Semaphore : in out Semaphore_P);


  type Monitor is abstract tagged limited private;
  procedure Seize_For_Reading (The_Monitor : in out Monitor)
    is abstract;
  procedure Seize_For_Writing (The_Monitor : in out Monitor)
    is abstract;
  procedure Release_From_Reading (The_Monitor : in out Monitor)
    is abstract;
  procedure Release_From_Writing (The_Monitor : in out Monitor)
    is abstract;

  type Monitor_P is access all Monitor'Class;
  procedure Delete (The_Monitor : in out Monitor_P);


  type Single_Monitor is new Monitor with private;
  procedure Seize_For_Reading (The_Monitor : in out Single_Monitor);
  procedure Seize_For_Writing (The_Monitor : in out Single_Monitor);
  procedure Release_From_Reading (The_Monitor : in out Single_Monitor);
  procedure Release_From_Writing (The_Monitor : in out Single_Monitor);


  type Multiple_Monitor is new Monitor with private;
  procedure Seize_For_Reading (The_Monitor : in out Multiple_Monitor);
  procedure Seize_For_Writing (The_Monitor : in out Multiple_Monitor);
  procedure Release_From_Reading (The_Monitor : in out Multiple_Monitor);
  procedure Release_From_Writing (The_Monitor : in out Multiple_Monitor);


  type Lock (Using : access Semaphore)
  is new Ada.Finalization.Limited_Controlled with private;
  procedure Initialize (The_Lock : in out Lock);
  procedure Finalize (The_Lock : in out Lock);


  type Read_Lock (Using : access Monitor'Class)
  is new Ada.Finalization.Limited_Controlled with private;
  procedure Initialize (The_Lock : in out Read_Lock);
  procedure Finalize (The_Lock : in out Read_Lock);


  type Write_Lock (Using : access Monitor'Class)
  is new Ada.Finalization.Limited_Controlled with private;
  procedure Initialize (The_Lock : in out Write_Lock);
  procedure Finalize (The_Lock : in out Write_Lock);


private

  type Monitor is abstract tagged limited null record;

  type Single_Monitor is new Monitor with record
    The_Semaphore : Semaphore;
  end record;

  -- Monitor_Type is due to Matthew Heaney <matthew_heaney@acm.org>.
  -- The Booch C++ version was inoperative, at least in sjw's translation.

  type Seize_Kind is (For_Reading, For_Writing);

  protected type Monitor_Type is
    entry Seize (Kind : Seize_Kind);
    procedure Release_For_Reading;
    procedure Release_For_Writing;
  private
    entry Waiting_To_Write;
    Reader_Count : Natural := 0;
    Writing : Boolean := False;
  end Monitor_Type;

  type Multiple_Monitor is new Monitor with record
    M : Monitor_Type;
  end record;

  type Lock (Using : access Semaphore)
  is new Ada.Finalization.Limited_Controlled with null record;

  type Read_Lock (Using : access Monitor'Class)
  is new Ada.Finalization.Limited_Controlled with null record;

  type Write_Lock (Using : access Monitor'Class)
  is new Ada.Finalization.Limited_Controlled with null record;

end BC.Support.Synchronization;
