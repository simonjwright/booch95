-- Copyright (C) 1994-1998 Grady Booch and Simon Wright.
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

with BC.Graphs;
with BC.Graphs.Directed;
with BC.Graphs.Undirected;
with Global_Heap;

package Graph_Test_Support is

  package AG is new BC.Graphs
     (Vertex_Item => Character,
      Arc_Item => Character,
      Storage_Manager => Global_Heap.Pool,
      Storage => Global_Heap.Storage);

  package DG is new AG.Directed;

  package UG is new AG.Undirected;

end Graph_Test_Support;
