-- Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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
with BC.Containers.Queues;
with BC.Containers.Queues.Bounded;
with BC.Containers.Queues.Dynamic;
with BC.Containers.Queues.Unbounded;
with Global_Heap;

package Queue_Test_Support is

  package Containers is new BC.Containers (Item => Character);

  package Queues is new Containers.Queues;

  package QB is new Queues.Bounded (Maximum_Size => 100);

  package QD is new Queues.Dynamic (Initial_Size => 10,
                                    Storage_Manager => Global_Heap.Pool,
                                    Storage => Global_Heap.Storage);

  package QU is new Queues.Unbounded (Storage_Manager => Global_Heap.Pool,
                                      Storage => Global_Heap.Storage);

end Queue_Test_Support;
