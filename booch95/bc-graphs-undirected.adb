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

with BC.Support.Exceptions;

package body BC.Graphs.Undirected is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Graphs.Undirected");


  ---------------------------------
  -- Undirected_Graph operations --
  ---------------------------------

  procedure Create_Arc (G : in out Undirected_Graph;
                        A : in out Undirected_Arc'Class;
                        I : Arc_Item;
                        First : in out Undirected_Vertex'Class;
                        Second : in out Undirected_Vertex'Class) is
  begin
    Clear (A);
    A.Rep := new Arc_Node'(Ada.Finalization.Controlled with
                           Item => I,
                           Enclosing => G'Unchecked_access,
                           From => First.Rep,
                           To => Second.Rep,
                           Next_Incoming => null,
                           Next_Outgoing => null,
                           Count => 1);
    if Second.Rep /= null then
      A.Rep.Next_Incoming := Second.Rep.Incoming;
      Second.Rep.Incoming := A.Rep;
      A.Rep.Count := A.Rep.Count + 1;
      Second.Rep.Count := Second.Rep.Count + 1;
    end if;
    if First.Rep /= null then
      A.Rep.Next_Outgoing := First.Rep.Outgoing;
      First.Rep.Outgoing := A.Rep;
      A.Rep.Count := A.Rep.Count + 1;
      First.Rep.Count := First.Rep.Count + 1;
    end if;
  end Create_Arc;


  ----------------------------------
  -- Undirected_Vertex operations --
  ----------------------------------

  function Arity (V : Undirected_Vertex) return Natural is
    Count : Natural := 0;
    Curr : Arc_Node_Ptr;
  begin
    Assert (V.Rep /= null,
            BC.Is_Null'Identity,
            "Arity",
            BSE.Is_Null);
    Curr := V.Rep.Incoming;
    while Curr /= null loop
      Count := Count + 1;
      Curr := Curr.Next_Incoming;
    end loop;
    Curr := V.Rep.Outgoing;
    while Curr /= null loop
      if Curr.From /= Curr.To then
        Count := Count + 1;
      end if;
      Curr := Curr.Next_Outgoing;
    end loop;
    return Count;
  end Arity;


  -------------------------------
  -- Undirected_Arc operations --
  -------------------------------

  procedure Set_First_Vertex (A : in out Undirected_Arc;
			      V : access Undirected_Vertex'Class) is
    Prev, Curr : Arc_Node_Ptr;
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "Set_First_Vertex",
            BSE.Is_Null);
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
    if V.Rep /= null then
      A.Rep.Next_Outgoing := V.Rep.Outgoing;
      V.Rep.Outgoing := A.Rep;
      A.Rep.Count := A.Rep.Count + 1;
      V.Rep.Count := V.Rep.Count + 1;
    end if;
    A.Rep.From := V.Rep;
  end Set_First_Vertex;


  procedure Set_Second_Vertex (A : in out Undirected_Arc;
			       V : access Undirected_Vertex'Class) is
    Prev, Curr : Arc_Node_Ptr;
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "Set_From_Vertex",
            BSE.Is_Null);
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
    if V.Rep /= null then
      A.Rep.Next_Incoming := V.Rep.Incoming;
      V.Rep.Incoming := A.Rep;
      A.Rep.Count := A.Rep.Count + 1;
      V.Rep.Count := V.Rep.Count + 1;
    end if;
    A.Rep.To := V.Rep;
  end Set_Second_Vertex;


  procedure First_Vertex (A : Undirected_Arc;
			  V : in out Undirected_Vertex'Class) is
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "First_Vertex",
            BSE.Is_Null);
    Clear (V);
    V.Rep := A.Rep.From;
    if V.Rep /= null then
      V.Rep.Count := V.Rep.Count + 1;
    end if;
  end First_Vertex;


  procedure Second_Vertex (A : Undirected_Arc;
			   V : in out Undirected_Vertex'Class) is
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "Second_Vertex",
            BSE.Is_Null);
    Clear (V);
    V.Rep := A.Rep.To;
    if V.Rep /= null then
      V.Rep.Count := V.Rep.Count + 1;
    end if;
  end Second_Vertex;


  --------------------------------
  -- Undirected_Graph iterators --
  --------------------------------

  procedure Reset (It : in out Undirected_Graph_Iterator) is
  begin
    It.Index := It.G.Rep;
  end Reset;


  procedure Next (It : in out Undirected_Graph_Iterator) is
  begin
    if It.Index /= null then
      It.Index := It.Index.Next;
    end if;
  end Next;


  function Is_Done (It : Undirected_Graph_Iterator) return Boolean is
  begin
    return It.Index = null;
  end Is_Done;


  procedure Current_Item (It : Undirected_Graph_Iterator;
			  V : in out Undirected_Vertex) is
  begin
    Assert (It.Index /= null,
            BC.Is_Null'Identity,
            "Current_Item(Undirected_Graph_Iterator)",
            BSE.Is_Null);
    Clear (V);
    V.Rep := It.Index;
    V.Rep.Count := V.Rep.Count + 1;
  end Current_Item;


  function Visit_Vertices
     (It : access Passive_Undirected_Graph_Iterator) return Boolean is
    Iter : Undirected_Graph_Iterator (It.G);
    V : Undirected_Vertex;
    Result : Boolean := True;
  begin
    while not Is_Done (Iter) loop
      Current_Item (Iter, V);
      Apply (V, Result);
      exit when not Result;
      Next (Iter);
    end loop;
    return Result;
  end Visit_Vertices;


  ---------------------------------
  -- Undirected_Vertex iterators --
  ---------------------------------

  procedure Reset (It : in out Undirected_Vertex_Iterator) is
  begin
    It.First := True;
    if It.V.Rep /= null then
      It.Index := It.V.Rep.Outgoing;
      if It.Index = null then
        It.First := False;
        It.Index := It.V.Rep.Incoming;
        while It.Index /= null and then (It.Index.From = It.Index.To) loop
          It.Index := It.Index.Next_Incoming;
        end loop;
      end if;
    else
      It.Index := null;
    end if;
  end Reset;


  procedure Next (It : in out Undirected_Vertex_Iterator) is
  begin
    -- XXX I think we ought to check here that there is an Index!
    if It.First then
      It.Index := It.Index.Next_Outgoing;
      if It.Index = null then
        It.First := False;
        It.Index := It.V.Rep.Incoming;
        while It.Index /= null and then (It.Index.From = It.Index.To) loop
          It.Index := It.Index.Next_Incoming;
        end loop;
      end if;
    elsif It.Index /= null then
      It.Index := It.Index.Next_Incoming;
      while It.Index /= null and then (It.Index.From = It.Index.To) loop
        It.Index := It.Index.Next_Incoming;
      end loop;
    end if;
  end Next;


  function Is_Done (It : Undirected_Vertex_Iterator) return Boolean is
  begin
    return It.Index = null;
  end Is_Done;


  procedure Current_Item (It : Undirected_Vertex_Iterator;
			  A : in out Undirected_Arc'Class) is
  begin
    Assert (It.Index /= null,
            BC.Is_Null'Identity,
            "Current_Item(Undirected_Vertex_Iterator)",
            BSE.Is_Null);
    Clear (A);
    A.Rep := It.Index;
    A.Rep.Count := A.Rep.Count + 1;
  end Current_Item;


  function Visit_Arcs
     (It : access Passive_Undirected_Vertex_Iterator) return Boolean is
    Iter : Undirected_Vertex_Iterator (It.V);
    A : Undirected_Arc;
    Result : Boolean := True;
  begin
    while not Is_Done (Iter) loop
      Current_Item (Iter, A);
      Apply (A, Result);
      exit when not Result;
      Next (Iter);
    end loop;
    return Result;
  end Visit_Arcs;


  ----------------------------------------------
  -- Utilities, controlled storage management --
  ----------------------------------------------

  procedure Initialize (It : in out Undirected_Vertex_Iterator) is
  begin
    Reset (It);
  end Initialize;


end BC.Graphs.Undirected;
