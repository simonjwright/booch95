-- Copyright (C) 1999-2001 Simon Wright.
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

-- This package defines sets of characters where equality is
-- case-independent.

with BC.Containers;
with BC.Containers.Sets;
with BC.Containers.Sets.Bounded;
with BC.Containers.Sets.Dynamic;
with BC.Containers.Sets.Unbounded;
with Global_Heap;

package User_Set_Support is

  type User_Character is new Character;
  function "=" (L, R : User_Character) return Boolean;

  package Containers is new BC.Containers (Item => User_Character);

  package Sets is new Containers.Sets;

  function User_Char_Hash (C : User_Character) return Natural;

  package SB is new Sets.Bounded (Hash => User_Char_Hash,
                                  Buckets => 3,
                                  Size => 100);

  package SD is new Sets.Dynamic (Hash => User_Char_Hash,
                                  Buckets => 3,
                                  Storage_Manager => Global_Heap.Pool,
                                  Storage => Global_Heap.Storage);

  package SU is new Sets.Unbounded (Hash => User_Char_Hash,
                                    Buckets => 3,
                                    Storage_Manager => Global_Heap.Pool,
                                    Storage => Global_Heap.Storage);

end User_Set_Support;
