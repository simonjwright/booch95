-- Copyright (C) 1994-2000 Grady Booch and Simon Wright.
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
with BC.Containers.Deques;
-- with BC.Containers.Deques.Bounded;
-- with BC.Containers.Deques.Dynamic;
with BC.Containers.Deques.Unbounded;
with Global_Heap;

package Deque_Test_Support is

  package Containers is new BC.Containers (Item => Character);

  package Deques is new Containers.Deques;

--   package DB is new Deques.Bounded (Maximum_Size => 100);

--   package DD is new Deques.Dynamic (Storage_Manager => Global_Heap.Pool,
--                                     Storage => Global_Heap.Storage);

  package DU is new Deques.Unbounded (Storage_Manager => Global_Heap.Pool,
                                      Storage => Global_Heap.Storage);

end Deque_Test_Support;
