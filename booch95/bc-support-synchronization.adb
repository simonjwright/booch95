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

with Ada.Unchecked_Deallocation;

package body BC.Support.Synchronization is

  -- Semaphore_Base --

  procedure Delete (The_Semaphore : in out Semaphore_P) is
    procedure Free is new Ada.Unchecked_Deallocation
       (Semaphore_Base'Class, Semaphore_P);
  begin
    Free (The_Semaphore);
  end Delete;

  -- Semaphore --

  protected body Semaphore_Type is

    entry Seize when True is
      use type Ada.Task_Identification.Task_Id;
    begin
      -- Why do I say Semaphore_Type.Seize rather than just Seize?
      --
      -- Jean-Pascal Cozic <Jean-Pascal.Cozic@ingsud.dga.fr> says:
      -- You have to write Semaphore_Type.Seize'Caller because there
      -- are two homographs Seize in the scope (the entry and the
      -- procedure). Using the context, there is no ambiguity because
      -- Caller deals with a prefix that denotes an entry_declaration,
      -- but the compiler cannot use context in this case.
      --
      -- See LRM 4.1.4(14).
      if Owner = Semaphore_Type.Seize'Caller then
        Count := Count + 1;
      else
        requeue Waiting with abort;
      end if;
    end Seize;

    procedure Release is
    begin
      Count := Count - 1;
    end Release;

    function None_Pending return Boolean is
    begin
      return Waiting'Count = 0;
    end None_Pending;

    entry Waiting when Count = 0 is
    begin
      Owner := Waiting'Caller;
      Count := 1;
    end Waiting;

  end Semaphore_Type;

  procedure Initialize (The_Semaphore : in out Semaphore) is
  begin
    The_Semaphore.S := new Semaphore_Type;
  end Initialize;

  procedure Finalize (The_Semaphore : in out Semaphore) is
    procedure Free is new Ada.Unchecked_Deallocation
       (Semaphore_Type, Semaphore_Type_P);
  begin
    Free (The_Semaphore.S);
  end Finalize;

  procedure Seize (The_Semaphore : in out Semaphore) is
  begin
    The_Semaphore.S.Seize;
  end Seize;

  procedure Release (The_Semaphore : in out Semaphore) is
  begin
    The_Semaphore.S.Release;
  end Release;

  function None_Pending (On_The_Semaphore : Semaphore) return Boolean is
  begin
    return On_The_Semaphore.S.None_Pending;
  end None_Pending;

  -- Monitor --

  procedure Delete (The_Monitor : in out Monitor_P) is
    procedure Free is new Ada.Unchecked_Deallocation
       (Monitor_Base'Class, Monitor_P);
  begin
    Free (The_Monitor);
  end Delete;

  -- Single_Monitor --

  procedure Seize_For_Reading (The_Monitor : in out Single_Monitor) is
  begin
    Seize (The_Monitor.The_Semaphore);
  end ;

  procedure Seize_For_Writing (The_Monitor : in out Single_Monitor) is
  begin
    Seize (The_Monitor.The_Semaphore);
  end ;

  procedure Release_From_Reading (The_Monitor : in out Single_Monitor) is
  begin
    Release (The_Monitor.The_Semaphore);
  end ;

  procedure Release_From_Writing (The_Monitor : in out Single_Monitor) is
  begin
    Release (The_Monitor.The_Semaphore);
  end ;

  -- Multiple_Monitor --

  protected body Monitor_Type is

    entry Seize (Kind : in Seize_Kind)
    when Waiting_To_Write'Count = 0 and not Writing is
    begin
      case Kind is
        when For_Reading =>
          Reader_Count := Reader_Count + 1;
        when For_Writing =>
          requeue Waiting_To_Write;
      end case;
    end Seize;

    procedure Release_For_Reading is
    begin
      Reader_Count := Reader_Count - 1;
    end;

    procedure Release_For_Writing is
    begin
      Writing := False;
    end;

    entry Waiting_To_Write when Reader_Count = 0 is
    begin
      Writing := True;
    end;

  end Monitor_Type;

  procedure Seize_For_Reading (The_Monitor : in out Multiple_Monitor) is
  begin
    The_Monitor.M.Seize (Kind => For_Reading);
  end ;

  procedure Seize_For_Writing (The_Monitor : in out Multiple_Monitor) is
  begin
    The_Monitor.M.Seize (Kind => For_Writing);
  end ;

  procedure Release_From_Reading (The_Monitor : in out Multiple_Monitor) is
  begin
    The_Monitor.M.Release_For_Reading;
  end ;

  procedure Release_From_Writing (The_Monitor : in out Multiple_Monitor) is
  begin
    The_Monitor.M.Release_For_Writing;
  end ;

  -- Lock --

  procedure Initialize (The_Lock : in out Lock) is
  begin
    Seize (The_Lock.Using.all);
  end ;

  procedure Finalize (The_Lock : in out Lock) is
  begin
    Release (The_Lock.Using.all);
  end ;

  -- Read_Lock --

  procedure Initialize (The_Lock : in out Read_Lock) is
  begin
    Seize_For_Reading (The_Lock.Using.all);
  end ;

  procedure Finalize (The_Lock : in out Read_Lock) is
  begin
    Release_From_Reading (The_Lock.Using.all);
  end ;

  -- Write_Lock --

  procedure Initialize (The_Lock : in out Write_Lock) is
  begin
    Seize_For_Writing (The_Lock.Using.all);
  end ;

  procedure Finalize (The_Lock : in out Write_Lock) is
  begin
    Release_From_Writing (The_Lock.Using.all);
  end ;


end BC.Support.Synchronization;
