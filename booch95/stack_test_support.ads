--  Copyright (C) 1994-1998,2001 Grady Booch and Simon Wright.
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
with BC.Containers.Stacks;
with BC.Containers.Stacks.Bounded;
with BC.Containers.Stacks.Dynamic;
with BC.Containers.Stacks.Unbounded;
with BC.Support.Standard_Storage;
with Global_Heap;

package Stack_Test_Support is

   package Containers is new BC.Containers (Item => Character);

   package Stacks is new Containers.Stacks;

   package SB is new Stacks.Bounded
     (Maximum_Size => 100);

   package SD is new Stacks.Dynamic
     (Storage => Global_Heap.Storage);

   package SU is new Stacks.Unbounded
     (Storage => BC.Support.Standard_Storage.Pool);

end Stack_Test_Support;
