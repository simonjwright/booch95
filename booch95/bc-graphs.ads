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
with BC.Smart;
with System.Storage_Pools;

generic
  type Vertex_Item is private;
  type Arc_Item is private;
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Graphs is

  -- A directed graph is an unrooted collection of vertices and directed
  -- arcs where cycles and cross-references are not permitted. An
  -- undirected graph is an unrooted collection of vertices and undirected
  -- arcs where cycles and cross-references are permitted. Three types
  -- collaborate to form the abstraction of a directed and undirected
  -- graph: a graph type, a vertex type, and an arc type.

  -- Directed and undirected graphs are monolithic structures although
  -- copying, assignment, and equality are prohibited. Their vertices and
  -- arcs are polylithic structures, and hence the semantics of copying,
  -- assignment, and equality involve structural sharing. Care must be
  -- taken in manipulating the same vertex or arc named by more than one
  -- alias.

  -- These classes are not intended to be subclassed.

  -- These abstractions have been carefully constructed to eliminate all
  -- storage leaks, except in the case of intentional abuses. When a graph
  -- is manipulated, all items that become unreachable are automatically
  -- reclaimed. Furthermore, this design protects against dangling
  -- references: an item is never reclaimed if there exists a reference to
  -- it.

  -- Each vertex and arc is a member of exactly one graph; furthermore, the
  -- vertices at both ends of an arc are guaranteed to be members of the
  -- same graph as the arc. This guarantee is provided by an implementation
  -- strategy whereby every graph is given a unique identity, and each
  -- vertex and arc is created only in the context of a particular graph.

  -- Note that objects of type Vertex, Arc are handles or references to the
  -- actual Graph components.

  -- Note that Containers contain just one sort of Item; Graphs aren't
  -- therefore Containers.

  type Graph is abstract new Ada.Finalization.Limited_Controlled with private;
  type Graph_Ptr is access all Graph'Class;

  type Vertex is abstract new Ada.Finalization.Controlled with private;

  type Arc is abstract new Ada.Finalization.Controlled with private;

  ----------------------
  -- Graph operations --
  ----------------------

  procedure Clear (G : in out Graph);
  -- Destroy all the vertices in the graph, and by implication, all the
  -- arcs in the graph. The semantics of destroy are such that any aliased
  -- vertices and arcs are not eliminated from the graph, because to do so
  -- would introduce dangling references.

  procedure Create_Vertex (G : in out Graph;
                           V : in out Vertex'Class;
                           I : Vertex_Item);
  -- Create a new vertex and add it to the graph, setting the second
  -- argument of this function as an alias to this new vertex.

  -- Arc creation is provided in concrete derivations.

  procedure Destroy_Vertex (G : in out Graph;
                            V : in out Vertex'Class);
  -- Destroy the given vertex and any associated arcs. If the vertex has no
  -- other aliases, eliminate it from the graph.

  procedure Destroy_Arc (G : in out Graph;
                         A : in out Arc'Class);
  -- Destroy the given arc and any associated vertices. If the arc has no
  -- other aliases, eliminate it from the graph.

  function Number_Of_Vertices (G : Graph) return Natural;
  -- Return the number of vertices in the graph.

  function Is_Empty (G : Graph) return Boolean;
  -- Return True if and only if the graph does not contain any vertices or
  -- arcs.

  function Is_Member (G : Graph; V : Vertex'Class) return Boolean;
  -- Return True if and only if the given vertex is not null and denotes a
  -- vertex in the graph.

  function Is_Member (G : Graph; A : Arc'Class) return Boolean;
  -- Return True if and only if the given arc is not null and denotes an
  -- arc in the graph.

  -----------------------
  -- Vertex operations --
  -----------------------

  function "=" (L, R : Vertex) return Boolean;
  -- Return True if and only if both vertices are null or are an alias to
  -- the same vertex.

  procedure Clear (V : in out Vertex);
  -- If the vertex is not null, remove this alias.

  procedure Set_Item (V : in out Vertex; I : Vertex_Item);
  -- Set the item of the given vertex.

  function Is_Null (V : Vertex) return Boolean;
  -- Return True if and only if the vertex is null.

  function Is_Shared (V : Vertex) return Boolean;
  -- Return True if and only if the vertex has other aliases.

  function Item (V : Vertex) return Vertex_Item;
  -- Return the item associated with the vertex.

  generic
    with procedure Process (I : in out Vertex_Item);
  procedure Access_Vertex_Item (V : Vertex'Class);
  -- Process has read-write access to the item associated with the vertex.

  function Enclosing_Graph (V : Vertex) return Graph_Ptr;
  -- Return the graph enclosing the vertex.

  --------------------
  -- Arc operations --
  --------------------

  function "=" (L, R : Arc) return Boolean;
  -- Return True if and only if both arcs are null or are an alias to the
  -- same arc.

  procedure Clear (A : in out Arc);
  -- If the arc is not null, remove this alias.

  procedure Set_Item (A : in out Arc; I : Arc_Item);
  -- Set the item of the given arc.

  function Is_Null (A : Arc) return Boolean;
  -- Return 1 if and only if the arc is null.

  function Is_Shared (A : Arc) return Boolean;
  -- Return True if and only if the arc has other aliases.

  function Item (A : Arc) return Arc_Item;
  -- Return the item associated with the arc.

  generic
    with procedure Process (I : in out Arc_Item);
  procedure Access_Arc_Item (A : Arc'Class);
  -- Process has read-write access to the item associated with the arc.

  function Enclosing_Graph (A : Arc) return Graph_Ptr;
  -- Return the graph enclosing the arc.

  --------------------------------------------
  -- Iteration over the Vertices in a Graph --
  --------------------------------------------

  -- Active iteration

  type Graph_Iterator is private;

  function New_Graph_Iterator (For_The_Graph : Graph) return Graph_Iterator
    is abstract;
  -- Return a reset Graph_Iterator bound to the specific Graph.

  procedure Reset (Obj : in out Graph_Iterator);
  -- Reset the Graph_Iterator to the beginning.

  procedure Next (Obj : in out Graph_Iterator);
  -- Advance the Graph_Iterator to the next Vertex in the Graph.

  function Is_Done (Obj : Graph_Iterator) return Boolean;
  -- Return True if there are no more Vertices in the Graph.

  function Current_Vertex (Obj : Graph_Iterator) return Vertex'Class;
  -- Return a handle on the current Vertex.

  -- Passive iteration

  generic
    with procedure Apply (Elem : in Vertex'Class; OK : out Boolean);
  procedure Visit_Vertices (Over_The_Graph : Graph'Class);
  -- Call Apply with a handle on each Vertex in the Graph. The iteration
  -- will terminate early if Apply sets OK to False.

  ---------------------------------------------------
  -- Iteration over the Arcs connected to a Vertex --
  ---------------------------------------------------

  -- Active iteration

  type Vertex_Iterator is private;

  function New_Vertex_Iterator (For_The_Vertex : Vertex) return Vertex_Iterator
    is abstract;
  -- Return a reset Vertex_Iterator bound to the specific Vertex.

  procedure Reset (Obj : in out Vertex_Iterator);
  -- Reset the Vertex_Iterator to the beginning.

  procedure Next (Obj : in out Vertex_Iterator);
  -- Advance the Vertex_Iterator to the next Arc in the Vertex.

  function Is_Done (Obj : Vertex_Iterator) return Boolean;
  -- Return True if there are no more Arcs in the Vertex.

  function Current_Arc (Obj : Vertex_Iterator) return Arc'Class;
  -- Return a handle on the current Arc.

  -- Passive iteration

  generic
    with procedure Apply (Elem : in Arc'Class; OK : out Boolean);
  procedure Visit_Arcs (Over_The_Vertex : Vertex'Class);
  -- Call Apply with a handle on each Arc in the Vertex. The iteration
  -- will terminate early if Apply sets OK to False.

private

  type Vertex_Node;
  type Vertex_Node_Ptr is access Vertex_Node;
  for Vertex_Node_Ptr'Storage_Pool use Storage;
  type Arc_Node;
  type Arc_Node_Ptr is access Arc_Node;
  for Arc_Node_Ptr'Storage_Pool use Storage;

  -- A Vertex Node is a simple node consisting of an item, a pointer to the
  -- enclosing graph, a pointer to the next vertex, pointers to the
  -- outgoing and incoming arcs, and a reference count
  -- XXX controlled only for check at finalization
  type Vertex_Node is new Ada.Finalization.Controlled with record
    Item : Vertex_Item;
    Enclosing : Graph_Ptr;
    Incoming : Arc_Node_Ptr;
    Outgoing : Arc_Node_Ptr;
    Next : Vertex_Node_Ptr;
    Count : Natural;
  end record;
  procedure Clear_Vertex_Node (G : in out Graph'Class;
                               N : in out Vertex_Node_Ptr);
  procedure Finalize (V : in out Vertex_Node);

  -- An Arc Node is a simple node consisting of an item, a pointer to the
  -- enclosing graph, a pointer to the next arc, pointers to the vertices
  -- to and from the arc, and a reference count
  -- XXX controlled only for check at finalization
  type Arc_Node is new Ada.Finalization.Controlled with record
    Item : Arc_Item;
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

  -- Actual_Graph_Iterators are strongly dependent on the concrete Graph
  -- implementation. The externally-visible Graph_Iterator is implemented as
  -- a (smart) pointer to the specific Graph's Actual_Graph_Iterator.
  --
  -- All the primitive subprograms of Graph_Iterator are implemented in terms
  -- of matching subprograms of Actual_Graph_Iterator.

  type Actual_Graph_Iterator (For_The_Graph : access Graph'Class)
  is abstract new Ada.Finalization.Limited_Controlled with null record;

  type Graph_Iterator_P is access Actual_Graph_Iterator'Class;

  type Vertex_Ptr is access Vertex'Class;

  procedure Reset (Obj : in out Actual_Graph_Iterator) is abstract;

  procedure Next (Obj : in out Actual_Graph_Iterator) is abstract;

  function Is_Done (Obj : Actual_Graph_Iterator) return Boolean is abstract;

  function Current_Vertex (Obj : Actual_Graph_Iterator) return Vertex'Class
    is abstract;

  package GSP is new BC.Smart (T => Actual_Graph_Iterator'Class,
                               P => Graph_Iterator_P);

  type Graph_Iterator is new GSP.Pointer;

  -- Actual_Vertex_Iterators are strongly dependent on the concrete Vertex
  -- implementation. The externally-visible Vertex_Iterator is implemented as
  -- a (smart) pointer to the specific Vertex's Actual_Vertex_Iterator.
  --
  -- All the primitive subprograms of Vertex_Iterator are implemented in terms
  -- of matching subprograms of Actual_Vertex_Iterator.

  type Actual_Vertex_Iterator (For_The_Vertex : access Vertex'Class)
  is abstract new Ada.Finalization.Limited_Controlled with null record;

  type Vertex_Iterator_P is access Actual_Vertex_Iterator'Class;

  type Arc_Ptr is access Arc'Class;

  procedure Reset (Obj : in out Actual_Vertex_Iterator) is abstract;

  procedure Next (Obj : in out Actual_Vertex_Iterator) is abstract;

  function Is_Done (Obj : Actual_Vertex_Iterator) return Boolean is abstract;

  function Current_Arc (Obj : Actual_Vertex_Iterator) return Arc'Class
    is abstract;

  package VSP is new BC.Smart (T => Actual_Vertex_Iterator'Class,
                               P => Vertex_Iterator_P);

  type Vertex_Iterator is new VSP.Pointer;


end BC.Graphs;
