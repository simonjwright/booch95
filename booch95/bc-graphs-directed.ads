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

  type Directed_Graph is new Graph with private;
  type Directed_Vertex is new Vertex with private;
  type Directed_Arc is new Arc with private;

  -------------------------------
  -- Directed_Graph operations --
  -------------------------------

  procedure Create_Arc (G : in out Directed_Graph;
                        A : in out Directed_Arc'Class;
                        I : Arc_Item;
                        From : in out Directed_Vertex'Class;
                        To : in out Directed_Vertex'Class);
  -- Create a new arc between the given vertices and add it to the graph,
  -- setting the second argument of this function as an alias to this new
  -- arc.

  --------------------------------
  -- Directed_Vertex operations --
  --------------------------------

  function Number_Of_Incoming_Arcs (V : Directed_Vertex) return Natural;
  -- Return the number of arcs directed to the vertex.

  function Number_Of_Outgoing_Arcs (V : Directed_Vertex) return Natural;
  -- Return the number of arcs emerging from the vertex.

  -----------------------------
  -- Directed_Arc operations --
  -----------------------------

  procedure Set_From_Vertex (A : in out Directed_Arc;
                             V : access Directed_Vertex'Class);
  -- Change the source of the arc to the given vertex. This change requires
  -- that the arc be removed from the collection of outgoing arcs in the
  -- original source vertex and added to the collection of outgoing arcs in
  -- the given source vertex.

  procedure Set_To_Vertex (A : in out Directed_Arc;
                           V : access Directed_Vertex'Class);
  -- Change the destination of the arc to the given vertex. This change
  -- requires that the arc be removed from the collection of incoming arcs
  -- in the original destination vertex and added to the collection of
  -- incoming arcs in the given destination vertex.

  procedure From_Vertex (A : Directed_Arc; V : in out Directed_Vertex'Class);
  -- Return the source vertex of the arc.

  procedure To_Vertex (A : Directed_Arc; V : in out Directed_Vertex'Class);
  -- Return the destination vertex of the arc.

  -----------------------
  -- Iteration support --
  -----------------------

  function New_Graph_Iterator (For_The_Graph : Directed_Graph)
                               return Graph_Iterator;
  -- Return a reset Iterator bound to the specific Graph.

  function New_Vertex_Iterator (For_The_Vertex : Directed_Vertex)
                                return Vertex_Iterator;
  -- Return a reset Iterator bound to the specific Vertex.

private

  type Directed_Graph is new Graph with null record;
  type Directed_Vertex is new Vertex with null record;
  type Directed_Arc is new Arc with null record;

  type Directed_Graph_Iterator (D : access Directed_Graph)
     is new Actual_Graph_Iterator (D) with record
    Index : Vertex_Node_Ptr := D.Rep;
  end record;

  procedure Initialize (It : in out Directed_Graph_Iterator);

  procedure Reset (It : in out Directed_Graph_Iterator);

  procedure Next (It : in out Directed_Graph_Iterator);

  function Is_Done (It : Directed_Graph_Iterator) return Boolean;

  function Current_Vertex (It : Directed_Graph_Iterator) return Vertex'Class;

  type Directed_Vertex_Iterator (D : access Directed_Vertex)
     is new Actual_Vertex_Iterator (D) with record
    Index : Arc_Node_Ptr;
  end record;

  procedure Initialize (It : in out Directed_Vertex_Iterator);

  procedure Reset (It : in out Directed_Vertex_Iterator);

  procedure Next (It : in out Directed_Vertex_Iterator);

  function Is_Done (It : Directed_Vertex_Iterator) return Boolean;

  function Current_Arc (It : Directed_Vertex_Iterator) return Arc'Class;

end BC.Graphs.Directed;
