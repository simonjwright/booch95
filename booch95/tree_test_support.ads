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

--  $Id$

with BC.Containers;
with BC.Containers.Trees;
with BC.Containers.Trees.AVL;
with BC.Containers.Trees.Binary;
with BC.Containers.Trees.Multiway;
with Global_Heap;

package Tree_Test_Support is

   package Containers is new BC.Containers (Item => Character);

   package Trees is new Containers.Trees;

   package TA is new Trees.AVL (Storage => Global_Heap.Storage);

   package TB is new Trees.Binary (Storage => Global_Heap.Storage);

   package TM is new Trees.Multiway (Storage => Global_Heap.Storage);

end Tree_Test_Support;
