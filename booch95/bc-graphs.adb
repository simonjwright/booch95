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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with BC.Support.Exceptions;

with Ada.Text_Io;

package body BC.Graphs is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Graphs");

  procedure Delete is new Ada.Unchecked_Deallocation
     (Vertex_Node, Vertex_Node_Ptr);
  procedure Delete is new Ada.Unchecked_Deallocation
     (Arc_Node, Arc_Node_Ptr);


  ----------------------
  -- Graph operations --
  ----------------------

  procedure Clear (G : in out Graph) is
    Curr : Vertex_Node_Ptr := G.Rep;
    Next : Vertex_Node_Ptr;
  begin
    -- In the C++, this was done using Iterators which created a Vertex and
    -- then called Destroy_Vertex.  We can't do that, because our Vertices
    -- are abstract.
    while Curr /= null loop
      Next := Curr.Next;
      Clear_Vertex_Node (G, Curr);
      Curr := Next;
    end loop;
  end Clear;


  procedure Create_Vertex (G : in out Graph;
                           V : in out Vertex'Class;
                           I : Vertex_Item) is
  begin
    Clear (V);
    V.Rep := new Vertex_Node'(Ada.Finalization.Controlled with
                              Item => I,
                              Enclosing => G'Unchecked_Access,
                              Incoming => null,
                              Outgoing => null,
                              Next => G.Rep,
                              Count => 1);
    G.Rep := V.Rep;
    G.Rep.Count := G.Rep.Count + 1;
  end Create_Vertex;


  procedure Destroy_Vertex (G : in out Graph;
                            V : in out Vertex'Class) is
  begin
    Assert (Is_Member (G, V),
            BC.Not_Found'Identity,
            "Destroy_Vertex",
            BSE.Disjoint);
    if V.Rep /= null then
      -- The C++ had the body of what is now Clear_Vertex_Node here,
      -- because it had the iterators available for thr Clear (Graph)
      -- operation.  Also, the type Vertex wasn't abstract (GEB didn't make
      -- much use of inheritance).
      Clear_Vertex_Node (G, V.Rep);
      Clear (V);
    end if;
  end Destroy_Vertex;


  procedure Destroy_Arc (G : in out Graph;
                         A : in out Arc'Class) is
    Prev, Curr : Arc_Node_Ptr;
  begin
    Assert (Is_Member (G, A),
            BC.Not_Found'Identity,
            "Destroy_Arc",
            BSE.Disjoint);
    if A.Rep /= null then
      if A.Rep.To /= null then
        Prev := null;
        Curr := A.Rep.To.Incoming;
        while Curr /= A.Rep loop
          Prev := Curr;
          Curr := Curr.Next_Incoming;
        end loop;
        if Prev = null then
          A.Rep.To.Incoming := Curr.Next_Incoming;
        else
          Prev.Next_Incoming := Curr.Next_Incoming;
        end if;
        A.Rep.To.Count := A.Rep.To.Count - 1;
        A.Rep.Count := A.Rep.Count - 1;
      end if;
      if A.Rep.From /= null then
        Prev := null;
        Curr := A.Rep.From.Outgoing;
        while Curr /= A.Rep loop
          Prev := Curr;
          Curr := Curr.Next_Outgoing;
        end loop;
        if Prev = null then
          A.Rep.From.Outgoing := Curr.Next_Outgoing;
        else
          Prev.Next_Outgoing := Curr.Next_Outgoing;
        end if;
        A.Rep.From.Count := A.Rep.From.Count - 1;
        A.Rep.Count := A.Rep.Count - 1;
      end if;
      A.Rep.From := null;
      A.Rep.To := null;
      A.Rep.Next_Incoming := null;
      A.Rep.Next_Outgoing := null;
      A.Rep.Enclosing := null;
      -- XXX should we decrement the count one more, like Destroy_Vertex?
      -- (presumably for the lost Enclosing?)
      Clear (A);
    end if;
  end Destroy_Arc;


  function Number_Of_Vertices (G : Graph) return Natural is
    Count : Natural := 0;
    Curr : Vertex_Node_Ptr := G.Rep;
  begin
    while Curr /= null loop
      Curr := Curr.Next;
      Count := Count + 1;
    end loop;
    return Count;
  end Number_Of_Vertices;


  function Is_Empty (G : Graph) return Boolean is
  begin
    return G.Rep = null;
  end Is_Empty;


  function Is_Member (G : Graph; V : Vertex'Class) return Boolean is
    -- Thanks to Tucker Taft for this workround to an access level problem
    type Graph_Const_Ptr is access constant Graph;
  begin
    if V.Rep = null then
      return False;
    else
      return Graph_Const_Ptr (V.Rep.Enclosing) = G'Access;
    end if;
  end Is_Member;


  function Is_Member (G : Graph; A : Arc'Class) return Boolean is
    -- Thanks to Tucker Taft for this workround to an access level problem
    type Graph_Const_Ptr is access constant Graph;
  begin
    if A.Rep = null then
      return False;
    else
      return Graph_Const_Ptr (A.Rep.Enclosing) = G'Access;
    end if;
  end Is_Member;


  -----------------------
  -- Vertex operations --
  -----------------------

  function "=" (L, R : Vertex) return Boolean is
  begin
    return L.Rep = R.Rep;
  end "=";


  procedure Clear (V : in out Vertex) is
  begin
    if V.Rep /= null then
      if V.Rep.Count > 1 then
        V.Rep.Count := V.Rep.Count - 1;
      else
        Delete (V.Rep);
      end if;
      V.Rep := null;
    end if;
  end Clear;


  procedure Set_Item (V : in out Vertex; I : Vertex_Item) is
  begin
    Assert (V.Rep /= null,
            BC.Is_Null'Identity,
            "Set_Item(Vertex)",
            BSE.Is_Null);
    V.Rep.Item := I;
  end Set_Item;


  function Is_Null (V : Vertex) return Boolean is
  begin
    return V.Rep = null;
  end Is_Null;


  function Is_Shared (V : Vertex) return Boolean is
  begin
    return V.Rep /= null and then V.Rep.Count > 1;
  end Is_Shared;


  function Item (V : Vertex) return Vertex_Item is
  begin
    Assert (V.Rep /= null,
            BC.Is_Null'Identity,
            "Item(Vertex)",
            BSE.Is_Null);
    return V.Rep.Item;
  end Item;


  function Item (V : Vertex) return Vertex_Item_Ptr is
  begin
    Assert (V.Rep /= null,
            BC.Is_Null'Identity,
            "Item(Vertex)",
            BSE.Is_Null);
    return V.Rep.Item'Access;
  end Item;


  function Enclosing_Graph (V : Vertex) return Graph_Ptr is
  begin
    Assert (V.Rep /= null,
            BC.Is_Null'Identity,
            "Enclosing_Graph(Vertex)",
            BSE.Is_Null);
    return V.Rep.Enclosing;
  end Enclosing_Graph;


  --------------------
  -- Arc operations --
  --------------------

  function "=" (L, R : Arc) return Boolean is
  begin
    return L.Rep = R.Rep;
  end "=";


  procedure Clear (A : in out Arc) is
  begin
    if A.Rep /= null then
      if A.Rep.Count > 1 then
        A.Rep.Count := A.Rep.Count - 1;
      else
        Delete (A.Rep);
      end if;
      A.Rep := null;
    end if;
  end Clear;


  procedure Set_Item (A : in out Arc; I : Arc_Item) is
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "Set_Item(Arc)",
            BSE.Is_Null);
    A.Rep.Item := I;
  end Set_Item;


  function Is_Null (A : Arc) return Boolean is
  begin
    return A.Rep = null;
  end Is_Null;


  function Is_Shared (A : Arc) return Boolean is
  begin
    return A.Rep /= null and then A.Rep.Count > 1;
  end Is_Shared;


  function Item (A : Arc) return Arc_Item is
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "Item(Arc)",
            BSE.Is_Null);
    return A.Rep.Item;
  end Item;


  function Item (A : Arc) return Arc_Item_Ptr is
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "Item(Arc)",
            BSE.Is_Null);
    return A.Rep.Item'Access;
  end Item;


  function Enclosing_Graph (A : Arc) return Graph_Ptr is
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "Enclosing_Graph(Arc)",
            BSE.Is_Null);
    return A.Rep.Enclosing;
  end Enclosing_Graph;


  ---------------------
  -- Graph iterators --
  ---------------------

  procedure Reset (It : in out Graph_Iterator) is
  begin
    It.Index := It.G.Rep;
  end Reset;


  procedure Next (It : in out Graph_Iterator) is
  begin
    if It.Index /= null then
      It.Index := It.Index.Next;
    end if;
  end Next;


  function Is_Done (It : Graph_Iterator) return Boolean is
  begin
    return It.Index = null;
  end Is_Done;


  procedure Current_Item (It : Graph_Iterator; V : in out Vertex'Class) is
  begin
    Assert (It.Index /= null,
            BC.Is_Null'Identity,
            "Current_Item(Graph_Iterator)",
            BSE.Is_Null);
    Clear (V);
    V.Rep := It.Index;
    V.Rep.Count := V.Rep.Count + 1;
  end Current_Item;


  ----------------------------------------------
  -- Utilities, controlled storage management --
  ----------------------------------------------

  procedure Clear_Vertex_Node (G : in out Graph'Class;
                               N : in out Vertex_Node_Ptr) is
    Curr : Arc_Node_Ptr;
    Prev, Index : Vertex_Node_Ptr;
  begin
    while N.Incoming /= null loop
      Curr := N.Incoming;
      N.Incoming := Curr.Next_Incoming;
      Curr.To := null;
      Curr.Next_Incoming := null;
      Curr.Enclosing := null;
      if Curr.Count > 1 then
        Curr.Count := Curr.Count - 1;
      else
        Delete (Curr);
      end if;
      N.Count := N.Count - 1;
    end loop;
    while N.Outgoing /= null loop
      Curr := N.Outgoing;
      N.Outgoing := Curr.Next_Outgoing;
      Curr.From := null;
      Curr.Next_Outgoing := null;
      Curr.Enclosing := null;
      if Curr.Count > 1 then
        Curr.Count := Curr.Count - 1;
      else
        Delete (Curr);
      end if;
      N.Count := N.Count - 1;
    end loop;
    Prev := null;
    Index := G.Rep;
    while Index /= N loop
      Prev := Index;
      Index := Index.Next;
    end loop;
    if Prev = null then
      G.Rep := Index.Next;
    else
      Prev.Next := Index.Next;
    end if;
    Index.Next := null;
    N.Enclosing := null;
    N.Count := N.Count - 1;
    if N.Count = 0 then
      Delete (N);
    end if;
  end Clear_Vertex_Node;


  procedure Finalize (V : in out Vertex_Node) is
  begin
    if V.Count > 1 then
      Ada.Text_Io.Put_Line ("Vertex_Node finalized with Count"
                            & Integer'Image (V.Count));
    end if;
  end Finalize;


  procedure Finalize (A : in out Arc_Node) is
  begin
    if A.Count > 1 then
      Ada.Text_Io.Put_Line ("Arc_Node finalized with Count"
                            & Integer'Image (A.Count));
    end if;
  end Finalize;


  procedure Finalize (G : in out Graph) is
  begin
    Clear (G);
  end Finalize;


  procedure Adjust (V : in out Vertex) is
  begin
    if V.Rep /= null then
      V.Rep.Count := V.Rep.Count + 1;
    end if;
  end Adjust;


  procedure Finalize (V : in out Vertex) is
    Curr : Arc_Node_Ptr;
  begin
    if V.Rep /= null then
      if V.Rep.Count > 1 then
        V.Rep.Count := V.Rep.Count - 1;
      else
        while V.Rep.Incoming /= null loop
          Curr := V.Rep.Incoming;
          V.Rep.Incoming := Curr.Next_Incoming;
          Curr.To := null;
          Curr.Next_Incoming := null;
          Curr.Enclosing := null;
          if Curr.Count > 1 then
            Curr.Count := Curr.Count - 1;
          else
            Delete (Curr);
          end if;
        end loop;
        while V.Rep.Outgoing /= null loop
          Curr := V.Rep.Outgoing;
          V.Rep.Outgoing := Curr.Next_Outgoing;
          Curr.From := null;
          Curr.Next_Outgoing := null;
          Curr.Enclosing := null;
          if Curr.Count > 1 then
            Curr.Count := Curr.Count - 1;
          else
            Delete (Curr);
          end if;
        end loop;
        Clear (V);
      end if;
    end if;
  end Finalize;


  procedure Adjust (A : in out Arc) is
  begin
    if A.Rep /= null then
      A.Rep.Count := A.Rep.Count + 1;
    end if;
  end Adjust;


  procedure Finalize (A : in out Arc) is
    Prev, Curr : Arc_Node_Ptr;
  begin
    if A.Rep /= null then
      if A.Rep.Count > 1 then
        A.Rep.Count := A.Rep.Count - 1;
      else
        if A.Rep.To /= null then
          Prev := null;
          Curr := A.Rep.To.Incoming;
          while Curr /= A.Rep loop
            Prev := Curr;
            Curr := Curr.Next_Incoming;
          end loop;
          if Prev = null then
            A.Rep.To.Incoming := Curr.Next_Incoming;
          else
            Prev.Next_Incoming := Curr.Next_Incoming;
          end if;
          if A.Rep.To.Count > 1 then
            A.Rep.To.Count := A.Rep.To.Count - 1;
          else
            Delete (A.Rep.To);
          end if;
          A.Rep.Count := A.Rep.Count - 1;
        end if;
        if A.Rep.From /= null then
          Prev := null;
          Curr := A.Rep.From.Outgoing;
          while Curr /= A.Rep loop
            Prev := Curr;
            Curr := Curr.Next_Outgoing;
          end loop;
          if Prev = null then
            A.Rep.From.Outgoing := Curr.Next_Outgoing;
          else
            Prev.Next_Outgoing := Curr.Next_Outgoing;
          end if;
          if A.Rep.From.Count > 1 then
            A.Rep.From.Count := A.Rep.From.Count - 1;
          else
            Delete (A.Rep.From);
          end if;
          -- XXX bug in C++ here?
          A.Rep.Count := A.Rep.Count - 1;
        end if;
        Clear (A);
      end if;
    end if;
  end Finalize;


end BC.Graphs;
