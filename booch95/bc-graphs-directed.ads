-- Copyright (C) 1994-1998 Grady Booch, David Weller and Simon Wright.
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
package BC.Graphs.Directed is

  type Graph is new Graphs.Graph with private;
  type Vertex is new Graphs.Vertex with private;
  type Arc is new Graphs.Arc with private;

private

  type Graph is new Graphs.Graph with null record;
  type Vertex is new Graphs.Vertex with null record;
  type Arc is new Graphs.Arc with null record;

end BC.Graphs.Directed;
