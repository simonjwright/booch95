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

  ---------------------
  -- Graph iterators --
  ---------------------

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
