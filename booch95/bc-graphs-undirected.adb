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


  ----------------------
  -- Graph operations --
  ----------------------

  procedure Create_Arc (G : in out Graph;
                        A : in out Arc'Class;
                        I : Arc_Item;
                        First : in out Vertex'Class;
                        Second : in out Vertex'Class) is
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


  -----------------------
  -- Vertex operations --
  -----------------------

  function Arity (V : Vertex) return Natural is
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


  --------------------
  -- Arc operations --
  --------------------

  procedure Set_First_Vertex (A : in out Arc; V : access Vertex'Class) is
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


  procedure Set_Second_Vertex (A : in out Arc; V : access Vertex'Class) is
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


  procedure First_Vertex (A : Arc; V : in out Vertex'Class) is
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


  procedure Second_Vertex (A : Arc; V : in out Vertex'Class) is
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


end BC.Graphs.Undirected;
