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
with BC.Containers.Queues;
with BC.Containers.Queues.Ordered;
with BC.Containers.Queues.Ordered.Bounded;
with BC.Containers.Queues.Ordered.Dynamic;
with BC.Containers.Queues.Ordered.Unbounded;
with BC.Support.Standard_Storage;
with Global_Heap;

package Ordered_Queue_Test_Support is

   package Containers is new BC.Containers (Item => Character);

   package Base_Queues is new Containers.Queues;

   package Queues is new Base_Queues.Ordered;

   package QB is new Queues.Bounded
     (Maximum_Size => 100);

   package QD is new Queues.Dynamic
     (Storage => Global_Heap.Storage);

   package QU is new Queues.Unbounded
     (Storage => BC.Support.Standard_Storage.Pool);

end Ordered_Queue_Test_Support;
