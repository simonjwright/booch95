-- Copyright (C) 2000,2001 Simon Wright.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

package BC.Support.Synchronization.Debug is

  pragma Elaborate_Body;

  -- Use these types when you need logging of activity.  Each
  -- operation reports itself (using the address of the Semaphore or
  -- Monitor) before calling its parent operation.


  type Debug_Semaphore is new Semaphore with private;
  procedure Initialize (The_Semaphore : in out Debug_Semaphore);
  procedure Adjust (The_Semaphore : in out Debug_Semaphore);
  procedure Finalize (The_Semaphore : in out Debug_Semaphore);
  procedure Seize (The_Semaphore : in out Debug_Semaphore);
  procedure Release (The_Semaphore : in out Debug_Semaphore);


  type Debug_Recursive_Semaphore is new Recursive_Semaphore with private;
  procedure Initialize (The_Semaphore : in out Debug_Recursive_Semaphore);
  procedure Adjust (The_Semaphore : in out Debug_Recursive_Semaphore);
  procedure Finalize (The_Semaphore : in out Debug_Recursive_Semaphore);
  procedure Seize (The_Semaphore : in out Debug_Recursive_Semaphore);
  procedure Release (The_Semaphore : in out Debug_Recursive_Semaphore);


  type Debug_Single_Monitor is new Single_Monitor with private;
  procedure Seize_For_Reading (The_Monitor : in out Debug_Single_Monitor);
  procedure Seize_For_Writing (The_Monitor : in out Debug_Single_Monitor);
  procedure Release_From_Reading (The_Monitor : in out Debug_Single_Monitor);
  procedure Release_From_Writing (The_Monitor : in out Debug_Single_Monitor);


  type Debug_Multiple_Monitor is new Multiple_Monitor with private;
  procedure Seize_For_Reading (The_Monitor : in out Debug_Multiple_Monitor);
  procedure Seize_For_Writing (The_Monitor : in out Debug_Multiple_Monitor);
  procedure Release_From_Reading (The_Monitor : in out Debug_Multiple_Monitor);
  procedure Release_From_Writing (The_Monitor : in out Debug_Multiple_Monitor);


private

  type Debug_Semaphore is new Semaphore with null record;

  type Debug_Recursive_Semaphore is new Recursive_Semaphore with null record;

  type Debug_Single_Monitor is new Single_Monitor with null record;

  type Debug_Multiple_Monitor is new Multiple_Monitor with null record;

end BC.Support.Synchronization.Debug;
