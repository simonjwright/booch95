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

with BC.Containers;
with BC.Containers.Rings;
-- with BC.Containers.Rings.Bounded;
-- with BC.Containers.Rings.Dynamic;
with BC.Containers.Rings.Unbounded;
with BC.Containers.Guarded;
with BC.Containers.Rings.Unbounded.Synchronized;
with BC.Support.Synchronization;
with Global_Heap;

package Ring_Test_Support is

  package Containers is new BC.Containers (Item => Character);

  package Rings is new Containers.Rings;

--   package RB is new Rings.Bounded (Maximum_Size => 100);

--   package RD is new Rings.Dynamic (Storage_Manager => Global_Heap.Pool,
--                                     Storage => Global_Heap.Storage);

  package RU is new Rings.Unbounded (Storage_Manager => Global_Heap.Pool,
                                     Storage => Global_Heap.Storage);
  subtype Unbounded_Ring is RU.Unbounded_Ring;

  package RUG is new Containers.Guarded
     (Base_Container => RU.Unbounded_Ring,
      Semaphore => BC.Support.Synchronization.Semaphore);
  subtype Guarded_Unbounded_Ring is RUG.Guarded_Container;

  package RUS is new RU.Synchronized
     (Monitor => BC.Support.Synchronization.Single_Monitor);
  subtype Synchronized_Unbounded_Ring is RUS.Synchronized_Unbounded_Ring;

end Ring_Test_Support;
