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

package body BC.Graphs.Directed is

  package BSE renames BC.Support.Exceptions;
  procedure Assert
  is new BSE.Assert ("BC.Graphs.Direct");


  ----------------------
  -- Graph operations --
  ----------------------

  procedure Create_Arc (G : in out Graph;
                        A : in out Arc'Class;
                        I : Arc_Item;
                        From : in out Vertex'Class;
                        To : in out Vertex'Class) is
  begin
    Clear (A);
    A.Rep := new Arc_Node'(Ada.Finalization.Controlled with
                           Item => I,
                           Enclosing => G'Unchecked_Access,
                           From => From.Rep,
                           To => To.Rep,
                           Next_Incoming => null,
                           Next_Outgoing => null,
                           Count => 1);
    if To.Rep /= null then
      A.Rep.Next_Incoming := To.Rep.Incoming;
      To.Rep.Incoming := A.Rep;
      A.Rep.Count := A.Rep.Count + 1;
      To.Rep.Count := To.Rep.Count + 1;
    end if;
    if From.Rep /= null then
      A.Rep.Next_Outgoing := From.Rep.Outgoing;
      From.Rep.Outgoing := A.Rep;
      A.Rep.Count := A.Rep.Count + 1;
      From.Rep.Count := From.Rep.Count + 1;
    end if;
  end Create_Arc;


  -----------------------
  -- Vertex operations --
  -----------------------

  function Number_Of_Incoming_Arcs (V : Vertex) return Natural is
    Count : Natural := 0;
    Curr : Arc_Node_Ptr;
  begin
    Assert (V.Rep /= null,
            BC.Is_Null'Identity,
            "Number_Of_Incoming_Arcs",
            BSE.Is_Null);
    Curr := V.Rep.Incoming;
    while Curr /= null loop
      Count := Count + 1;
      Curr := Curr.Next_Incoming;
    end loop;
    return Count;
  end Number_Of_Incoming_Arcs;


  function Number_Of_Outgoing_Arcs (V : Vertex) return Natural is
    Count : Natural := 0;
    Curr : Arc_Node_Ptr;
  begin
    Assert (V.Rep /= null,
            BC.Is_Null'Identity,
            "Number_Of_Outgoing_Arcs",
            BSE.Is_Null);
    Curr := V.Rep.Outgoing;
    while Curr /= null loop
      Count := Count + 1;
      Curr := Curr.Next_Outgoing;
    end loop;
    return Count;
  end Number_Of_Outgoing_Arcs;


  --------------------
  -- Arc operations --
  --------------------

  procedure Set_From_Vertex (A : in out Arc; V : access Vertex'Class) is
    Prev, Curr : Arc_Node_Ptr;
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "Set_From_Vertex",
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
  end Set_From_Vertex;


  procedure Set_To_Vertex (A : in out Arc; V : access Vertex'Class) is
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
  end Set_To_Vertex;


  procedure From_Vertex (A : Arc; V : in out Vertex'Class) is
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "From_Vertex",
            BSE.Is_Null);
    Clear (V);
    V.Rep := A.Rep.From;
    if V.Rep /= null then
      V.Rep.Count := V.Rep.Count + 1;
    end if;
  end From_Vertex;


  procedure To_Vertex (A : Arc; V : in out Vertex'Class) is
  begin
    Assert (A.Rep /= null,
            BC.Is_Null'Identity,
            "To_Vertex",
            BSE.Is_Null);
    Clear (V);
    V.Rep := A.Rep.To;
    if V.Rep /= null then
      V.Rep.Count := V.Rep.Count + 1;
    end if;
  end To_Vertex;


  ---------------------
  -- Graph iterators --
  ---------------------

  function Visit_Vertices
     (It : access Passive_Graph_Iterator) return Boolean is
    Iter : Graph_Iterator (It.G);
    V : Vertex;
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


  ---------------------
  -- Vertex iterators --
  ---------------------

  procedure Reset (It : in out Vertex_Iterator) is
  begin
    if It.V.Rep /= null then
      It.Index := It.V.Rep.Outgoing;
    else
      It.Index := null;
    end if;
  end Reset;


  procedure Next (It : in out Vertex_Iterator) is
  begin
    if It.Index /= null then
      It.Index := It.Index.Next_Outgoing;
    end if;
  end Next;


  function Is_Done (It : Vertex_Iterator) return Boolean is
  begin
    return It.Index = null;
  end Is_Done;


  procedure Current_Item (It : Vertex_Iterator; A : in out Arc'Class) is
  begin
    Assert (It.Index /= null,
            BC.Is_Null'Identity,
            "Current_Item(Vertex_Iterator)",
            BSE.Is_Null);
    Clear (A);
    A.Rep := It.Index;
    A.Rep.Count := A.Rep.Count + 1;
  end Current_Item;


  function Visit_Arcs (It : access Passive_Vertex_Iterator) return Boolean is
    Iter : Vertex_Iterator (It.V);
    A : Arc;
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

  procedure Initialize (It : in out Vertex_Iterator) is
  begin
    if It.V.Rep /= null then
      It.Index := It.V.Rep.Outgoing;
    end if;
  end Initialize;


end BC.Graphs.Directed;
