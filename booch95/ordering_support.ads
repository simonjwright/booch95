--  Copyright (C) 1999,2001 Simon Wright.
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

with BC.Containers;
with BC.Containers.Collections;
with BC.Containers.Collections.Ordered;
with BC.Containers.Collections.Ordered.Unbounded;
with BC.Containers.Queues;
with BC.Containers.Queues.Ordered;
with BC.Containers.Queues.Ordered.Unbounded;
with Global_Heap;

package Ordering_Support is

   type Sortable is record
      Key : Integer;
      Ident : Integer;
   end record;

   function "<" (L, R : Sortable) return Boolean;

   package Containers is new BC.Containers (Item => Sortable);

   package Base_Collections is new Containers.Collections;

   package Collections is new Base_Collections.Ordered;

   package CU is new Collections.Unbounded
     (Storage_Manager => Global_Heap.Pool,
      Storage => Global_Heap.Storage);

   package Base_Queues is new Containers.Queues;

   package Queues is new Base_Queues.Ordered;

   package QU is new Queues.Unbounded (Storage_Manager => Global_Heap.Pool,
                                       Storage => Global_Heap.Storage);

end Ordering_Support;
