--  Copyright (C) 2001 Simon Wright.
--  All Rights Reserved.
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

--  $Id$

--  This package defines maps keyed by characters where equality is
--  case-independent.

with Ada.Strings.Unbounded;
with BC.Containers;
with BC.Containers.Maps;
with BC.Containers.Maps.Bounded;
with BC.Containers.Maps.Dynamic;
with BC.Containers.Maps.Unbounded;
with Global_Heap;

package User_Map_Support is

   type User_Character is new Character;
   function "=" (L, R : User_Character) return Boolean;

   package Containers is new BC.Containers
     (Item => Ada.Strings.Unbounded.Unbounded_String,
        "=" => Ada.Strings.Unbounded."=");

   package Maps is new Containers.Maps (Key => User_Character);

   function User_Char_Hash (C : User_Character) return Natural;

   package MB is new Maps.Bounded (Hash => User_Char_Hash,
                                   Buckets => 3,
                                   Maximum_Size => 100);

   package MD is new Maps.Dynamic (Hash => User_Char_Hash,
                                   Buckets => 3,
                                   Storage => Global_Heap.Storage);

   package MU is new Maps.Unbounded (Hash => User_Char_Hash,
                                     Buckets => 3,
                                     Storage => Global_Heap.Storage);

end User_Map_Support;
