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
with BC.Containers.Sets;
with BC.Containers.Sets.Bounded;
with BC.Containers.Sets.Dynamic;
with BC.Containers.Sets.Unbounded;
with Global_Heap;

package Set_Test_Support is

  package Containers is new BC.Containers (Item => Character);

  package Sets is new Containers.Sets;

  function Char_Hash (C : Character) return Natural;

  package SB is new Sets.Bounded (Hash => Char_Hash,
                                  Buckets => 3,
                                  Size => 100);

  package SD is new Sets.Dynamic (Hash => Char_Hash,
                                  Buckets => 3,
                                  Storage_Manager => Global_Heap.Pool,
                                  Storage => Global_Heap.Storage);

  package SU is new Sets.Unbounded (Hash => Char_Hash,
                                    Buckets => 3,
                                    Storage_Manager => Global_Heap.Pool,
                                    Storage => Global_Heap.Storage);

end Set_Test_Support;
