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
with BC.Containers.Rings.Unbounded.Guarded;
with BC.Containers.Rings.Unbounded.Synchronized;
with BC.Support.Synchronization;
with Global_Heap;

package Ring_Test_Support is

  package Containers is new BC.Containers (Item => Character);

  package Rings is new Containers.Rings;

--   package RB is new Rings.Bounded (Maximum_Size => 100);

--   package RD is new Rings.Dynamic (Storage_Manager => Global_Heap.Pool,
--                                     Storage => Global_Heap.Storage);

  package RUB is new Rings.Unbounded (Storage_Manager => Global_Heap.Pool,
                                     Storage => Global_Heap.Storage);

  package RUG is new RUB.Guarded
     (Semaphore => BC.Support.Synchronization.Semaphore);

  package RUS is new RUB.Synchronized
     (Monitor => BC.Support.Synchronization.Single_Monitor);

  package RU renames RUG;

end Ring_Test_Support;
