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

generic
  with procedure Apply (Elem : in Item; OK : out Boolean);
procedure BC.Containers.Trees.Multiway.Pre_Order
   (T : Multiway_Tree; Success : out Boolean);
-- Call Apply with a copy of each Item in the Tree, in preorder (for each
-- node, visit the node itself and then its children). The iteration will
-- terminate early if Apply sets OK to False.
