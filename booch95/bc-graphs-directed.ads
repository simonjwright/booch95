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

generic
package BC.Graphs.Directed is

  type Graph is new Graphs.Graph with private;
  type Vertex is new Graphs.Vertex with private;
  type Arc is new Graphs.Arc with private;

  ----------------------
  -- Graph operations --
  ----------------------

  procedure Create_Arc (G : in out Graph;
                        A : in out Arc'Class;
                        I : Arc_Item;
                        From : in out Vertex'Class;
                        To : in out Vertex'Class);

  -----------------------
  -- Vertex operations --
  -----------------------

  function Number_Of_Incoming_Arcs (V : Vertex) return Natural;

  function Number_Of_Outgoing_Arcs (V : Vertex) return Natural;

  --------------------
  -- Arc operations --
  --------------------

  procedure Set_From_Vertex (A : in out Arc; V : access Vertex'Class);

  procedure Set_To_Vertex (A : in out Arc; V : access Vertex'Class);

  procedure From_Vertex (A : Arc; V : in out Vertex'Class);

  procedure To_Vertex (A : Arc; V : in out Vertex'Class);

private

  type Graph is new Graphs.Graph with null record;
  type Vertex is new Graphs.Vertex with null record;
  type Arc is new Graphs.Arc with null record;

end BC.Graphs.Directed;
