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

with Ada.Text_Io;
with Graph_Test_Support;

procedure Graph_Test is

  use Ada.Text_Io;
  use Graph_Test_Support;

  procedure Assertion (B : Boolean;
                       S : String) is
  begin
    if not B then
      Put_Line (S);
    end if;
  end Assertion;

  procedure Test_Directed (G : in out DG.Graph;
                           V1, V2, V3 : in out DG.Vertex;
                           A1, A2, A3 : in out DG.Arc) is
    --| BC_TDirectedArc<Char, CPtr, BC_CManaged> a4;
    A4 : DG.Arc;
    use DG;
  begin
    --| assertion(g.IsEmpty(), "** D01: Graph is not initially empty");
    Assertion (Is_Empty (G), "** D01: Graph is not initially empty");
    --| assertion((g.NumberOfVertices() == 0), "** D02: Number of vertices is not correct");
    Assertion (Number_Of_Vertices (G) = 0,
               "** D02: Number of vertices is not correct");
    --| assertion(v1.IsNull(), "** D03: Vertex is not initially null");
    Assertion (Is_Null (V1),
               "** D03: Vertex is not initially null");
    --| assertion(a1.IsNull(), "** D04: Arc is not initially null");
    Assertion (Is_Null (A1),
               "** D04: Arc is not initially null");
    --| g.CreateVertex(v1, 'a');
    Create_Vertex (G, V1, 'a');
    --| g.CreateVertex(v2, 'b');
    Create_Vertex (G, V2, 'b');
    --| g.CreateVertex(v3, 'c');
    Create_Vertex (G, V3, 'c');
    --| g.CreateVertex(v3, 'd');
    Create_Vertex (G, V3, 'd');
    --| g.CreateVertex(v3, 'e');
    Create_Vertex (G, V3, 'e');
    --| g.CreateVertex(v3, 'f');
    Create_Vertex (G, V3, 'f');
    --| g.CreateVertex(v3, 'g');
    Create_Vertex (G, V3, 'g');
    --| assertion(!g.IsEmpty(), "** D05: Graph is empty");
    Assertion (not Is_Empty (G),
               "** D05: Graph is empty");
    --| assertion((g.NumberOfVertices() == 7), "** D06: Number of vertices is not correct");
    Assertion (Number_Of_Vertices (G) = 7,
               "** D06: Number of vertices is not correct");
    --| assertion(!v1.IsNull(), "** D07: Vertex is null");
    Assertion (not Is_Null (V1),
               "** D07: Vertex is null");
    --| assertion(v1.IsShared(), "** D08: Vertex is not shared");
    Assertion (Is_Shared (V1),
               "** D08: Vertex is not shared");
    --| assertion((v1.Item() == 'a'), "** D09: Vertex Item is not correct");
    Assertion (Item (V1) = 'a',
               "** D09: Vertex Item is not correct");
    --| assertion(g.IsMember(v1), "** D10: Vertex is not a member of the graph");
    Assertion (Is_Member (G, V1),
               "** D10: Vertex is not a member of the graph");
    --| assertion(!v3.IsNull(), "** D11: Vertex is null");
    Assertion (not Is_Null (V3),
               "** D11: Vertex is null");
    --| assertion(v3.IsShared(), "** D12: Vertex is not shared");
    Assertion (Is_Shared (V3),
               "** D12: Vertex is not shared");
    --| assertion((v3.Item() == 'g'), "** D13: Vertex Item is not correct");
    Assertion (Item (V3) = 'g',
               "** D13: Vertex Item is not correct");
    --| assertion(g.IsMember(v3), "** D14: Vertex is not a member of the graph");
    Assertion (Is_Member (G, V3),
               "** D14: Vertex is not a member of the graph");
    --| assertion((v1.EnclosingGraph().NumberOfVertices() == 7), "** D15: Number of vertices is not correct");
    Assertion (Number_Of_Vertices (DG.Graph (Enclosing_Graph (V1).all)) =7,
               "** D15: Number of vertices is not correct");
    --| v3 = v1;
    V3 := V1;
    --| assertion((v3.EnclosingGraph().NumberOfVertices() == 7), "** D16: Number of vertices is not correct");
    Assertion (Number_Of_Vertices (DG.Graph (Enclosing_Graph (V3).all)) =7,
               "** D16: Number of vertices is not correct");
    --| v3.Clear();
    Clear (V3);
    --| assertion(v3.IsNull(), "** D17: Vertex is null");
    Assertion (Is_Null (V3),
               "** D17: Vertex is not null");
    --| assertion(!v3.IsShared(), "** D18: Vertex is not shared");
    Assertion (not Is_Shared (V3),
               "** D18: Vertex is shared");
    --| assertion(!g.IsMember(v3), "** D19: Vertex is not a member of the graph");
    Assertion (not Is_Member (G, V3),
               "** D19: Vertex is a member of the graph");
    --| assertion(g.IsMember(v1), "** D20: Vertex is not a member of the graph");
    Assertion (Is_Member (G, V1),
               "** D20: Vertex is not a member of the graph");
    --| v3 = v1;
    V3 := V1;
    --| assertion((v1 == v3), "** D21: Vertices are not equal");
    Assertion (V1 = V3,
               "** D21: Vertices are not equal");
    --| v1.SetItem('A');
    Set_Item (V1, 'A');
    --| assertion((v3.Item() == 'A'), "** D22: Vertex Item is not correct");
    Assertion (Item (V3) = 'A',
               "** D22: Vertex Item is not correct");
    --| g.DestroyVertex(v1);
    Destroy_Vertex (G, V1);
    --| assertion((g.NumberOfVertices() == 6), "** D23: Number of vertices is not correct");
    Assertion (Number_Of_Vertices (G) = 6,
               "** D23: Number of vertices is not correct");
    --| assertion(!g.IsMember(v1), "** D24: Vertex is not a member of the graph");
    Assertion (not Is_Member (G, V1),
               "** D24: Vertex is a member of the graph");
    --| assertion(v1.IsNull(), "** D25: Vertex is null");
    Assertion (Is_Null (V1),
               "** D25: Vertex is not null");
    --| assertion(!v1.IsShared(), "** D26: Vertex is not shared");
    Assertion (not Is_Shared (V1),
               "** D26: Vertex is shared");
    --| assertion(!g.IsMember(v3), "** D27: Vertex is not a member of the graph");
    Assertion (not Is_Member (G, V3),
               "** D27: Vertex is a member of the graph");
    --| assertion(!v3.IsNull(), "** D28: Vertex is null");
    Assertion (not Is_Null (V3),
               "** D28: Vertex is null");
    --| assertion(!v3.IsShared(), "** D29: Vertex is not shared");
    Assertion (not Is_Shared (V3),
               "** D29: Vertex is shared");
    --| assertion((v1 != v3), "** D30: Vertices are not equal");
    Assertion (V1 /= V3,
               "** D30: Vertices are not equal");
    --| assertion(g.IsMember(v2), "** D31: Vertex is not a member of the graph");
    Assertion (Is_Member (G, V2),
               "** D31: Vertex is not a member of the graph");
    --| g.Clear();
    Clear (G);
    --| assertion((g.NumberOfVertices() == 0), "** D32: Number of vertices is not correct");
    Assertion (Number_Of_Vertices (G) = 0,
               "** D32: Number of vertices is not correct");
    --| assertion(!g.IsMember(v2), "** D33: Vertex is not a member of the graph");
    Assertion (not Is_Member (G, V2),
               "** D33: Vertex is a member of the graph");
    --| assertion(!v2.IsNull(), "** D34: Vertex is null");
    Assertion (not Is_Null (V2),
               "** D34: Vertex is null");
    --| assertion(!v2.IsShared(), "** D35: Vertex is not shared");
    Assertion (not Is_Shared (V2),
               "** D35: Vertex is shared");
    --| assertion((v2.NumberOfIncomingArcs() == 0), "** D36: Arity of incoming arcs is not correct");
    Assertion (Number_Of_Incoming_Arcs (V2) = 0,
               "** D36: Arity of incoming arcs is not correct");
    --| assertion((v2.NumberOfOutgoingArcs() == 0), "** D37: Arity of outgoing arcs is not correct");
    Assertion (Number_Of_Outgoing_Arcs (V2) = 0,
               "** D37: Arity of outgoing arcs is not correct");
    --| g.CreateVertex(v1, 'a');
    Create_Vertex (G, V1, 'a');
    --| g.CreateVertex(v2, 'b');
    Create_Vertex (G, V2, 'b');
    --| g.CreateVertex(v3, 'c')
    Create_Vertex (G, V3, 'c');
    --| g.CreateArc(a1, &gItems[0], v1, v2);
    Create_Arc (G, A1, '0', V1, V2); -- NB, these test the 'reference'
    --| g.CreateArc(a2, &gItems[1], v2, v3);
    Create_Arc (G, A2, '1', V2, V3);
    --| g.CreateArc(a4, &gItems[2], v3, v1);
    Create_Arc (G, A4, '2', V3, V1);
    --| g.CreateArc(a3, &gItems[3], v3, v2);
    Create_Arc (G, A3, '3', V3, V2);
    --| assertion(g.IsMember(a1), "** D38: Arc is not a member of the graph");
    Assertion (Is_Member (G, A1),
               "** D38: Arc is not a member of the graph");
    --| assertion((v2.NumberOfIncomingArcs() == 2), "** D39: Arity of incoming arcs is not correct");
    Assertion (Number_Of_Incoming_Arcs (V2) = 2,
               "** D39: Arity of incoming arcs is not correct");
    --| assertion((v3.NumberOfOutgoingArcs() == 2), "** D40: Arity of outgoing arcs is not correct");
    Assertion (Number_Of_Outgoing_Arcs (V3) = 2,
               "** D40: Arity of outgoing arcs is not correct");
    --| g.DestroyArc(a3);
    Destroy_Arc (G, A3);
    --| assertion(a3.IsNull(), "** D41: Vertex is null");
    Assertion (Is_Null (A3),
               "** D41: Arc is not null");
    --| assertion(!g.IsMember(a3), "** D42: Arc is not a member of the graph");
    Assertion (not Is_Member (G, A3),
               "** D42: Arc is a member of the graph");
    --| assertion((v2.NumberOfIncomingArcs() == 1), "** D43: Arity of incoming arcs is not correct");
    Assertion (Number_Of_Incoming_Arcs (V2) = 1,
               "** D43: Arity of incoming arcs is not correct");
    --| assertion((v3.NumberOfOutgoingArcs() == 1), "** D44: Arity of outgoing arcs is not correct");
    Assertion (Number_Of_Outgoing_Arcs (V3) = 1,
               "** D44: Arity of outgoing arcs is not correct");
    --| g.CreateArc(a3, &gItems[3], v3, v2);
    Create_Arc (G, A3, '3', V3, V2);
    --| a3.ToVertex(v1);
    To_Vertex (A3, V1);
    --| assertion((v1 == v2), "** D45: Vertices are not equal");
    Assertion (V1 = V2,
               "** D45: Vertices are not equal");
    --| a3.FromVertex(v1);
    From_Vertex (A3, V1);
    --| assertion((v1 == v3), "** D46: Vertices are not equal");
    Assertion (V1 = V3,
               "** D46: Vertices are not equal");
    --| a1.FromVertex(v1);
    From_Vertex (A1, V1);
    --| assertion((v1.Item() == 'a'), "** D47: Vertex Item is not correct");
    Assertion (Item (V1) = 'a',
               "** D47: Vertex Item is not correct");
    --| a3.SetToVertex(v1);
    Set_To_Vertex (A3, V1'Access);
    --| assertion((v1.NumberOfIncomingArcs() == 2), "** D48: Arity of incoming arcs is not correct");
    Assertion (Number_Of_Incoming_Arcs (V1) = 2,
               "** D48: Arity of incoming arcs is not correct");
    --| assertion((v2.NumberOfIncomingArcs() == 1), "** D49: Arity of incoming arcs is not correct");
    Assertion (Number_Of_Incoming_Arcs (V2) = 1,
               "** D49: Arity of incoming arcs is not correct");
    --| assertion((a3.Item() == &gItems[3]), "** D50: Arc Item is not correct");
    Assertion (Item (A3) = '3',
               "** D50: Arc Item is not correct");
    --| a3.SetItem(&gItems[4]);
    Set_Item (A3, '4');
    --| assertion((a3.Item() == &gItems[4]), "** D51: Arc Item is not correct");
    Assertion (Item (A3) = '4',
               "** D51: Arc Item is not correct");
    --| assertion((a3.EnclosingGraph().NumberOfVertices() == 3), "** D52: Number of vertices is not correct");
    Assertion (Number_Of_Vertices (DG.Graph (Enclosing_Graph (A3).all)) = 3,
               "** D52: Number of vertices is not correct");
    --| a2 = a3;
    A2 := A3;
    --| a2.Clear();
    Clear (A2);
    --| assertion(!g.IsMember(a2), "** D53: Arc is a member of the graph");
    Assertion (not Is_Member (G, A2),
               "** D53: Arc is a member of the graph");
    --| assertion(g.IsMember(a3), "** D54: Arc is not a member of the graph");
    Assertion (Is_Member (G, A3),
               "** D54: Arc is not a member of the graph");
    --| a3.SetFromVertex(v2);
    Set_From_Vertex (A3, V2'Access);
    --| assertion((v2.NumberOfOutgoingArcs() == 2), "** D55: Arity of outgoing arcs is not correct");
    Assertion (Number_Of_Outgoing_Arcs (V2) = 2,
               "** D55: Arity of outgoing arcs is not correct");
    --| assertion((v3.NumberOfOutgoingArcs() == 1), "** D56: Arity of outgoing arcs is not correct");
    Assertion (Number_Of_Outgoing_Arcs (V3) = 1,
               "** D56: Arity of outgoing arcs is not correct");
    --| g.CreateArc(a3, &gItems[7], v3, v3);
    Create_Arc (G, A3, '7', V3, V3);
    --| assertion((v3.NumberOfOutgoingArcs() == 2), "** D57: Arity of outgoing arcs is not correct");
    Assertion (Number_Of_Outgoing_Arcs (V3) = 2,
               "** D57: Arity of outgoing arcs is not correct");
    --| assertion((v3.NumberOfIncomingArcs() == 2), "** D58: Arity of outgoing arcs is not correct");
    Assertion (Number_Of_Incoming_Arcs (V3) = 2,
               "** D58: Arity of outgoing arcs is not correct");
    --| v3.SetItem('C');
    Set_Item (V3, 'C');
    --| g.DestroyVertex(v3);
    Destroy_Vertex (G, V3);
    --| assertion(v3.IsNull(), "** D59: Vertex is null");
    Assertion (Is_Null (V3),
               "** D59: Vertex is not null");
    --| assertion(!a3.IsNull(), "** D60: Arc is null");
    Assertion (not Is_Null (A3),
                "** D60: Arc is null");
    --| assertion((v1.NumberOfIncomingArcs() == 2), "** D61: Arity of outgoing arcs is not correct");
    Assertion (Number_Of_Incoming_Arcs (V1) = 2,
               "** D61: Arity of outgoing arcs is not correct");
    --| assertion((v2.NumberOfOutgoingArcs() == 2), "** D62: Arity of outgoing arcs is not correct");
    Assertion (Number_Of_Outgoing_Arcs (V2) = 2,
               "** D62: Arity of outgoing arcs is not correct");
    --| g.CreateVertex(v1, 'c');
    Create_Vertex (G, V1, 'c');
    --| g.CreateArc(a3, &gItems[5], v1, v1);
    Create_Arc (G, A3, '5', V1, V1);
    --| g.CreateArc(a3, &gItems[7], v1, v2);
    Create_Arc (G, A3, '7', V1, V2);
    --| g.CreateVertex(v1, 'd');
    Create_Vertex (G, V1, 'd');
    --| g.CreateArc(a3, &gItems[8], v1, v2);
    Create_Arc (G, A3, '8', V1, V2);
    --| a4.SetFromVertex(v1);
    Set_From_Vertex (A4, V1'Access);
  end Test_Directed;

  D_G : DG.Graph;
  D_V1, D_V2, D_V3 : DG.Vertex;
  D_A1, D_A2, D_A3 : DG.Arc;

begin
  Put_Line ("Starting graph tests");

  Put_Line ("...Directed Graph");
  Test_Directed (D_G, D_V1, D_V2, D_V3, D_A1, D_A2, D_A3);

  Put_Line ("Completed graph tests");

end Graph_Test;
