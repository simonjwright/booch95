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
  -- Create a new arc between the given vertices and add it to the graph,
  -- setting the second argument of this function as an alias to this new
  -- arc.

  -----------------------
  -- Vertex operations --
  -----------------------

  function Number_Of_Incoming_Arcs (V : Vertex) return Natural;
  -- Return the number of arcs directed to the vertex.

  function Number_Of_Outgoing_Arcs (V : Vertex) return Natural;
  -- Return the number of arcs emerging from the vertex.

  --------------------
  -- Arc operations --
  --------------------

  procedure Set_From_Vertex (A : in out Arc; V : access Vertex'Class);
  -- Change the source of the arc to the given vertex. This change requires
  -- that the arc be removed from the collection of outgoing arcs in the
  -- original source vertex and added to the collection of outgoing arcs in
  -- the given source vertex.

  procedure Set_To_Vertex (A : in out Arc; V : access Vertex'Class);
  -- Change the destination of the arc to the given vertex. This change
  -- requires that the arc be removed from the collection of incoming arcs
  -- in the original destination vertex and added to the collection of
  -- incoming arcs in the given destination vertex.

  procedure From_Vertex (A : Arc; V : in out Vertex'Class);
  -- Return the source vertex of the arc.

  procedure To_Vertex (A : Arc; V : in out Vertex'Class);
  -- Return the destination vertex of the arc.

  ---------------------
  -- Graph iterators --
  ---------------------

  type Graph_Iterator (G : access Graph) is limited private;

  procedure Reset (It : in out Graph_Iterator);

  procedure Next (It : in out Graph_Iterator);

  function Is_Done (It : Graph_Iterator) return Boolean;

  procedure Current_Item (It : Graph_Iterator; V : in out Vertex);


  type Passive_Graph_Iterator (G : access Graph) is limited private;

  generic
    with procedure Apply (V : Vertex; OK : out Boolean);
  function Visit_Vertices (It : access Passive_Graph_Iterator) return Boolean;

  ---------------------
  -- Vertex iterators --
  ---------------------

  type Vertex_Iterator (V : access Vertex'Class) is limited private;

  procedure Reset (It : in out Vertex_Iterator);

  procedure Next (It : in out Vertex_Iterator);

  function Is_Done (It : Vertex_Iterator) return Boolean;

  procedure Current_Item (It : Vertex_Iterator; A : in out Arc'Class);


  type Passive_Vertex_Iterator (V : access Vertex) is limited private;

  generic
    with procedure Apply (A : Arc; OK : out Boolean);
  function Visit_Arcs (It : access Passive_Vertex_Iterator) return Boolean;

private

  type Graph is new Graphs.Graph with null record;
  type Vertex is new Graphs.Vertex with null record;
  type Arc is new Graphs.Arc with null record;

  type Graph_Iterator (G : access Graph) is limited record
    Index : Vertex_Node_Ptr := G.Rep;
  end record;

  type Passive_Graph_Iterator (G : access Graph) is limited record
    Success : Boolean := False;
  end record;

  type Vertex_Iterator (V : access Vertex'Class)
  is new Ada.Finalization.Limited_Controlled with record
    Index : Arc_Node_Ptr;
  end record;
  procedure Initialize (It : in out Vertex_Iterator);

  type Passive_Vertex_Iterator (V : access Vertex) is limited record
    Success : Boolean := False;
  end record;

end BC.Graphs.Directed;
