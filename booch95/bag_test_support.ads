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
with BC.Containers.Bags;
with BC.Containers.Bags.Bounded;
with BC.Containers.Bags.Dynamic;
with BC.Containers.Bags.Unbounded;
with Global_Heap;

package Bag_Test_Support is

  package Containers is new BC.Containers (Item => Character);

  package Bags is new Containers.Bags;

  function Char_Hash (C : Character) return Positive;

  package BB is new Bags.Bounded (Hash => Char_Hash,
                                  Buckets => 3,
                                  Size => 100);

  package BD is new Bags.Dynamic (Hash => Char_Hash,
                                  Buckets => 3,
                                  Storage_Manager => Global_Heap.Pool,
                                  Storage => Global_Heap.Storage);

  package BU is new Bags.Unbounded (Hash => Char_Hash,
                                    Buckets => 3,
                                    Storage_Manager => Global_Heap.Pool,
                                    Storage => Global_Heap.Storage);

end Bag_Test_Support;
