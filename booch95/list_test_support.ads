-- Copyright (C) 1994-1998 Grady Booch and Simon Wright.
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
with BC.Containers.Lists;
with BC.Containers.Lists.Single;
with BC.Containers.Lists.Double;
with Global_Heap;

package List_Test_Support is

  package Containers is new BC.Containers (Item => Character);

  package Lists is new Containers.Lists;

  package LS is new Lists.Single (Storage_Manager => Global_Heap.Pool,
                                  Storage => Global_Heap.Storage);

  package LD is new Lists.Double (Storage_Manager => Global_Heap.Pool,
                                  Storage => Global_Heap.Storage);

end List_Test_Support;
