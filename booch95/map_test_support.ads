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
with BC.Containers.Maps;
with BC.Containers.Maps.Bounded;
with BC.Containers.Maps.Dynamic;
with BC.Containers.Maps.Unbounded;
with Chunks;
with Global_Heap;

package Map_Test_Support is

  package Containers is new BC.Containers (Item => Chunks.Chunk_Ptr,
                                           "=" => Chunks."=");

  package Maps is new Containers.Maps (Key => Character);

  function Char_Hash (C : Character) return Natural;

  package MB is new Maps.Bounded (Hash => Char_Hash,
                                  Buckets => 3,
                                  Maximum_Size => 100);

  package MD is new Maps.Dynamic (Hash => Char_Hash,
                                  Buckets => 3,
                                  Storage_Manager => Global_Heap.Pool,
                                  Storage => Global_Heap.Storage);

  package MU is new Maps.Unbounded (Hash => Char_Hash,
                                    Buckets => 3,
                                    Storage_Manager => Global_Heap.Pool,
                                    Storage => Global_Heap.Storage);

  Gitems : array (0 .. 9) of aliased Chunks.Chunk;

end Map_Test_Support;
