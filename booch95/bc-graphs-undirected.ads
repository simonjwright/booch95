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

  -----------------------
  -- Iteration support --
  -----------------------

  function New_Graph_Iterator (For_The_Graph : Undirected_Graph)
                               return Graph_Iterator;
  -- Return a reset Iterator bound to the specific Graph.

  function New_Vertex_Iterator (For_The_Vertex : Undirected_Vertex)
                                return Vertex_Iterator;
  -- Return a reset Iterator bound to the specific Vertex.

private

  type Undirected_Graph is new Graph with null record;
  type Undirected_Vertex is new Vertex with null record;
  type Undirected_Arc is new Arc with null record;

  type Undirected_Graph_Iterator (U : access Undirected_Graph)
     is new Actual_Graph_Iterator (U) with record
    Index : Vertex_Node_Ptr := U.Rep;
  end record;

  procedure Initialize (It : in out Undirected_Graph_Iterator);

  procedure Reset (It : in out Undirected_Graph_Iterator);

  procedure Next (It : in out Undirected_Graph_Iterator);

  function Is_Done (It : Undirected_Graph_Iterator) return Boolean;

  function Current_Vertex (It : Undirected_Graph_Iterator) return Vertex'Class;

  type Undirected_Vertex_Iterator (U : access Undirected_Vertex)
     is new Actual_Vertex_Iterator (U) with record
    Index : Arc_Node_Ptr;
    First : Boolean;
  end record;

  procedure Initialize (It : in out Undirected_Vertex_Iterator);

  procedure Reset (It : in out Undirected_Vertex_Iterator);

  procedure Next (It : in out Undirected_Vertex_Iterator);

  function Is_Done (It : Undirected_Vertex_Iterator) return Boolean;

  function Current_Arc (It : Undirected_Vertex_Iterator) return Arc'Class;

end BC.Graphs.Undirected;
