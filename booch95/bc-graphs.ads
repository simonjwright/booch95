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

with Ada.Finalization;

generic
  type Vertex_Item is private;
  type Vertex_Item_Ptr is access all Vertex_Item;
       -- XXX for function Item return Vertex_Item_Ptr
  type Arc_Item is private;
  type Arc_Item_Ptr is access all Arc_Item;
       -- XXX for function Item return Arc_Item_Ptr
package BC.Graphs is

  type Graph is abstract new Ada.Finalization.Limited_Controlled with private;
  type Graph_Ptr is access all Graph'Class;

  type Vertex is abstract new Ada.Finalization.Controlled with private;
  type Vertex_Ptr is access all Vertex'Class;

  type Arc is abstract new Ada.Finalization.Controlled with private;
  type Arc_Ptr is access all Arc'Class;

  ----------------------
  -- Graph operations --
  ----------------------

  procedure Clear (G : in out Graph);

  procedure Create_Vertex (G : in out Graph;
                           V : in out Vertex'Class;
                           I : Vertex_Item);

  procedure Destroy_Vertex (G : in out Graph;
                            V : in out Vertex'Class);

  procedure Destroy_Arc (G : in out Graph;
                         A : in out Arc'Class);

  function Number_Of_Vertices (G : Graph) return Natural;

  function Is_Empty (G : Graph) return Boolean;

  function Is_Member (G : Graph; V : Vertex'Class) return Boolean;

  function Is_Member (G : Graph; A : Arc'Class) return Boolean;

  -----------------------
  -- Vertex operations --
  -----------------------

  function "=" (L, R : Vertex) return Boolean;

  procedure Clear (V : in out Vertex);

  procedure Set_Item (V : in out Vertex; I : Vertex_Item);

  function Is_Null (V : Vertex) return Boolean;

  function Is_Shared (V : Vertex) return Boolean;

  function Item (V : Vertex) return Vertex_Item;

  function Item (V : Vertex) return Vertex_Item_Ptr;  -- hmm

  function Enclosing_Graph (V : Vertex) return Graph_Ptr;

  --------------------
  -- Arc operations --
  --------------------

  function "=" (L, R : Arc) return Boolean;

  procedure Clear (A : in out Arc);

  procedure Set_Item (A : in out Arc; I : Arc_Item);

  function Is_Null (A : Arc) return Boolean;

  function Is_Shared (A : Arc) return Boolean;

  function Item (A : Arc) return Arc_Item;

  function Item (A : Arc) return Arc_Item_Ptr;

  function Enclosing_Graph (A : Arc) return Graph_Ptr;

  ---------------------
  -- Graph iterators --
  ---------------------

  type Graph_Iterator (G : access Graph'Class) is limited private;

  procedure Reset (It : in out Graph_Iterator);

  procedure Next (It : in out Graph_Iterator);

  function Is_Done (It : Graph_Iterator) return Boolean;

  procedure Current_Item (It : Graph_Iterator; V : in out Vertex'Class);

  -- For passive iterator, see concrete Graph package (Directed, Undirected)

private

  type Vertex_Node;
  type Vertex_Node_Ptr is access Vertex_Node;
  type Arc_Node;
  type Arc_Node_Ptr is access Arc_Node;

  -- A Vertex Node is a simple node consisting of an item, a pointer to the
  -- enclosing graph, a pointer to the next vertex, pointers to the
  -- outgoing and incoming arcs, and a reference count
  -- XXX controlled only for check at finalization
  type Vertex_Node is new Ada.Finalization.Controlled with record
    Item : aliased Vertex_Item;  -- XXX for function Item return Item_Ptr
    Enclosing : Graph_Ptr;
    Incoming : Arc_Node_Ptr;
    Outgoing : Arc_Node_Ptr;
    Next : Vertex_Node_Ptr;
    Count : Natural;
  end record;
  procedure Clear_Vertex_Node (G : in out Graph'Class;
                               N : Vertex_Node_Ptr);
  procedure Finalize (V : in out Vertex_Node);

  -- An Arc Node is a simple node consisting of an item, a pointer to the
  -- enclosing graph, a pointer to the next arc, pointers to the vertices
  -- to and from the arc, and a reference count
  -- XXX controlled only for check at finalization
  type Arc_Node is new Ada.Finalization.Controlled with record
    Item : aliased Arc_Item;  -- XXX for function Item return Item_Ptr
    Enclosing : Graph_Ptr;
    From : Vertex_Node_Ptr;
    To : Vertex_Node_Ptr;
    Next_Incoming : Arc_Node_Ptr;
    Next_Outgoing : Arc_Node_Ptr;
    Count : Natural;
  end record;
  procedure Finalize (A : in out Arc_Node);

  type Graph is abstract new Ada.Finalization.Limited_Controlled with record
    Rep : Vertex_Node_Ptr;
  end record;
  procedure Finalize (G : in out Graph);

  type Vertex is abstract new Ada.Finalization.Controlled with record
    Rep : Vertex_Node_Ptr;
  end record;
  procedure Adjust (V : in out Vertex);
  procedure Finalize (V : in out Vertex);

  type Arc is abstract new Ada.Finalization.Controlled with record
    Rep : Arc_Node_Ptr;
  end record;
  procedure Adjust (A : in out Arc);
  procedure Finalize (A : in out Arc);

  type Graph_Iterator (G : access Graph'Class) is limited record
    Index : Vertex_Node_Ptr := G.Rep;
  end record;

end BC.Graphs;

