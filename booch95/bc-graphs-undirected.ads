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
package BC.Graphs.Undirected is

  type Undirected_Graph is new Graph with private;
  type Undirected_Vertex is new Vertex with private;
  type Undirected_Arc is new Arc with private;

  ---------------------------------
  -- Undirected_Graph operations --
  ---------------------------------

  procedure Create_Arc (G : in out Undirected_Graph;
                        A : in out Undirected_Arc'Class;
                        I : Arc_Item;
                        First : in out Undirected_Vertex'Class;
                        Second : in out Undirected_Vertex'Class);
  -- Create a new arc between the given vertices and add it to the graph,
  -- setting the second argument of this function as an alias to this new
  -- arc.

  ----------------------------------
  -- Undirected_Vertex operations --
  ----------------------------------

  function Arity (V : Undirected_Vertex) return Natural;
  -- Return the number of arcs connected to the vertex; self-arcs are
  -- counted only once.

  -------------------------------
  -- Undirected_Arc operations --
  -------------------------------

  procedure Set_First_Vertex (A : in out Undirected_Arc;
			      V : access Undirected_Vertex'Class);
  -- Change the first vertex of the arc to the given vertex. This change
  -- requires that the arc be removed from the collection of arcs in the
  -- original vertex and added to the collection of arcs in the given
  -- vertex.

  procedure Set_Second_Vertex (A : in out Undirected_Arc;
			       V : access Undirected_Vertex'Class);
  -- Change the second vertex of the arc to the given vertex. This change
  -- requires that the arc be removed from the collection of arcs in the
  -- original vertex and added to the collection of arcs in the given
  -- vertex.

  procedure First_Vertex (A : Undirected_Arc;
			  V : in out Undirected_Vertex'Class);
  -- Return the first vertex associated with the arc.

  procedure Second_Vertex (A : Undirected_Arc;
			   V : in out Undirected_Vertex'Class);
  -- Return the second vertex associated with the arc.

  --------------------------------
  -- Undirected_Graph iterators --
  --------------------------------

  type Undirected_Graph_Iterator (G : access Undirected_Graph)
  is limited private;

  procedure Reset (It : in out Undirected_Graph_Iterator);

  procedure Next (It : in out Undirected_Graph_Iterator);

  function Is_Done (It : Undirected_Graph_Iterator) return Boolean;

  procedure Current_Item (It : Undirected_Graph_Iterator;
			  V : in out Undirected_Vertex);


  type Passive_Undirected_Graph_Iterator (G : access Undirected_Graph)
  is limited private;

  generic
    with procedure Apply (V : Undirected_Vertex; OK : out Boolean);
  function Visit_Vertices
     (It : access Passive_Undirected_Graph_Iterator) return Boolean;

  ---------------------------------
  -- Undirected_Vertex iterators --
  ---------------------------------

  type Undirected_Vertex_Iterator (V : access Undirected_Vertex'Class)
  is limited private;

  procedure Reset (It : in out Undirected_Vertex_Iterator);

  procedure Next (It : in out Undirected_Vertex_Iterator);

  function Is_Done (It : Undirected_Vertex_Iterator) return Boolean;

  procedure Current_Item (It : Undirected_Vertex_Iterator;
			  A : in out Undirected_Arc'Class);


  type Passive_Undirected_Vertex_Iterator (V : access Undirected_Vertex)
  is limited private;

  generic
    with procedure Apply (A : Undirected_Arc; OK : out Boolean);
  function Visit_Arcs
     (It : access Passive_Undirected_Vertex_Iterator) return Boolean;

private

  type Undirected_Graph is new Graph with null record;
  type Undirected_Vertex is new Vertex with null record;
  type Undirected_Arc is new Arc with null record;

  type Undirected_Graph_Iterator (G : access Undirected_Graph)
  is limited record
    Index : Vertex_Node_Ptr := G.Rep;
  end record;

  type Passive_Undirected_Graph_Iterator (G : access Undirected_Graph)
  is limited record
    Success : Boolean := False;
  end record;

  type Undirected_Vertex_Iterator (V : access Undirected_Vertex'Class)
  is new Ada.Finalization.Limited_Controlled with record
    Index : Arc_Node_Ptr;
    First : Boolean;
  end record;
  procedure Initialize (It : in out Undirected_Vertex_Iterator);

  type Passive_Undirected_Vertex_Iterator (V : access Undirected_Vertex)
  is limited record
    Success : Boolean := False;
  end record;

end BC.Graphs.Undirected;
