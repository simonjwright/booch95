--  Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with BC.Containers;
with BC.Containers.Collections;
with BC.Containers.Collections.Ordered;
with BC.Containers.Collections.Ordered.Bounded;
with BC.Containers.Collections.Ordered.Dynamic;
with BC.Containers.Collections.Ordered.Unbounded;
with Global_Heap;

package Ordered_Collection_Test_Support is

   package Containers is new BC.Containers (Item => Character);

   package Base_Collections is new Containers.Collections;

   package Collections is new Base_Collections.Ordered;

   package CB is new Collections.Bounded
     (Maximum_Size => 100);

   package CD is new Collections.Dynamic
     (Storage_Manager => Global_Heap.Pool,
      Storage => Global_Heap.Storage);

   package CU is new Collections.Unbounded
     (Storage_Manager => Global_Heap.Pool,
      Storage => Global_Heap.Storage);

end Ordered_Collection_Test_Support;
