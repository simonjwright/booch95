--  Copyright (C) 1994-2001 Grady Booch and Simon Wright.
--  All Rights Reserved.
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with BC.Support.Exceptions;
with System.Address_To_Access_Conversions;

package body BC.Graphs.Directed is

   package BSE renames BC.Support.Exceptions;
   procedure Assert
   is new BSE.Assert ("BC.Graphs.Directed");


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
      Arc_Id := Arc_Id + 1;
      A.Rep := new Arc_Node'(Ada.Finalization.Controlled with
                             Item => I,
                             Enclosing => G'Unchecked_Access,
                             From => From.Rep,
                             To => To.Rep,
                             Next_Incoming => null,
                             Next_Outgoing => null,
                             Count => 1,
                             Id => Arc_Id);
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

   procedure Set_From_Vertex (A : in out Arc;
                              V : access Vertex'Class) is
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


   procedure Set_To_Vertex (A : in out Arc;
                            V : access Vertex'Class) is
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

   package Graph_Address_Conversions
   is new System.Address_To_Access_Conversions (Graph);

   function New_Graph_Iterator
     (For_The_Graph : Graph) return Graph_Iterator'Class is
   begin
      return Directed_Graph_Iterator'
        (For_The_Graph => Graph_Address_Conversions.To_Pointer
           (For_The_Graph'Address).all'Access,
         Index => For_The_Graph.Rep);
   end New_Graph_Iterator;


   package Vertex_Address_Conversions
   is new System.Address_To_Access_Conversions (Vertex);


   function New_Vertex_Iterator
     (For_The_Vertex : Vertex) return Vertex_Iterator'Class is
      Result : Vertex_Bothways_Iterator;
   begin
      Result.For_The_Vertex :=
        Vertex_Address_Conversions.To_Pointer
        (For_The_Vertex'Address).all'Access;
      Reset (Result);
      return Result;
   end New_Vertex_Iterator;


   function New_Vertex_Incoming_Iterator
     (For_The_Vertex : Vertex) return Vertex_Iterator'Class is
      Result : Vertex_Incoming_Iterator;
   begin
      Result.For_The_Vertex :=
        Vertex_Address_Conversions.To_Pointer
        (For_The_Vertex'Address).all'Access;
      Reset (Result);
      return Result;
   end New_Vertex_Incoming_Iterator;


   function New_Vertex_Outgoing_Iterator
     (For_The_Vertex : Vertex) return Vertex_Iterator'Class is
      Result : Vertex_Outgoing_Iterator;
   begin
      Result.For_The_Vertex :=
        Vertex_Address_Conversions.To_Pointer
        (For_The_Vertex'Address).all'Access;
      Reset (Result);
      return Result;
   end New_Vertex_Outgoing_Iterator;


   -------------------------------
   -- Private iteration support --
   -------------------------------

   procedure Reset (It : in out Directed_Graph_Iterator) is
   begin
      It.Index := It.For_The_Graph.Rep;
   end Reset;


   procedure Next (It : in out Directed_Graph_Iterator) is
   begin
      if It.Index /= null then
         It.Index := It.Index.Next;
      end if;
   end Next;


   function Is_Done (It : Directed_Graph_Iterator) return Boolean is
   begin
      return It.Index = null;
   end Is_Done;


   function Current_Vertex (It : Directed_Graph_Iterator)
                           return Abstract_Vertex'Class is
   begin
      Assert (It.Index /= null,
              BC.Is_Null'Identity,
              "Current_Vertex(Graph_Iterator)",
              BSE.Is_Null);
      It.Index.Count := It.Index.Count + 1;
      return Vertex'(Ada.Finalization.Controlled with Rep => It.Index);
   end Current_Vertex;


   ----------------------
   -- Vertex iterators --
   ----------------------

   --------------
   -- Abstract --
   --------------

   function Is_Done (It : Vertex_Abstract_Iterator) return Boolean is
   begin
      return It.Index = null;
   end Is_Done;


   function Current_Arc (It : Vertex_Abstract_Iterator)
                        return Abstract_Arc'Class is
   begin
      Assert (It.Index /= null,
              BC.Is_Null'Identity,
              "Current_Arc(Vertex_Outgoing_Iterator)",
              BSE.Is_Null);
      It.Index.Count := It.Index.Count + 1;
      return Arc'(Ada.Finalization.Controlled with Rep => It.Index);
   end Current_Arc;


   --------------
   -- Bothways --
   --------------

   procedure Reset (It : in out Vertex_Bothways_Iterator) is
   begin
      It.Do_Outgoing := True;
      if It.For_The_Vertex.Rep /= null then
         It.Index := It.For_The_Vertex.Rep.Outgoing;
         if It.Index = null then
            It.Do_Outgoing := False;
            It.Index := It.For_The_Vertex.Rep.Incoming;
            --  skip self-directed arcs, already seen in outgoing side
            --  XXX hmm, wouldn't .Outgoing have been non-null?
            while It.Index /= null and then (It.Index.From = It.Index.To) loop
               pragma Assert (False);
               It.Index := It.Index.Next_Incoming;
            end loop;
         end if;
      else
         It.Index := null;
      end if;
   end Reset;


   procedure Next (It : in out Vertex_Bothways_Iterator) is
   begin
      if It.Do_Outgoing then
         It.Index := It.Index.Next_Outgoing;
         if It.Index = null then
            It.Do_Outgoing := False;
            It.Index := It.For_The_Vertex.Rep.Incoming;
            --  skip self-directed arcs, already seen in outgoing side
            while It.Index /= null and then (It.Index.From = It.Index.To) loop
               It.Index := It.Index.Next_Incoming;
            end loop;
         end if;
      elsif It.Index /= null then
         It.Index := It.Index.Next_Incoming;
         --  skip self-directed arcs, already seen in outgoing side
         while It.Index /= null and then (It.Index.From = It.Index.To) loop
            It.Index := It.Index.Next_Incoming;
         end loop;
      end if;
   end Next;


   --------------
   -- Outgoing --
   --------------

   procedure Reset (It : in out Vertex_Outgoing_Iterator) is
   begin
      if It.For_The_Vertex.Rep /= null then
         It.Index := It.For_The_Vertex.Rep.Outgoing;
      else
         It.Index := null;
      end if;
   end Reset;


   procedure Next (It : in out Vertex_Outgoing_Iterator) is
   begin
      if It.Index /= null then
         It.Index := It.Index.Next_Outgoing;
      end if;
   end Next;


   --------------
   -- Incoming --
   --------------

   procedure Reset (It : in out Vertex_Incoming_Iterator) is
   begin
      if It.For_The_Vertex.Rep /= null then
         It.Index := It.For_The_Vertex.Rep.Incoming;
      else
         It.Index := null;
      end if;
   end Reset;


   procedure Next (It : in out Vertex_Incoming_Iterator) is
   begin
      if It.Index /= null then
         It.Index := It.Index.Next_Incoming;
      end if;
   end Next;


end BC.Graphs.Directed;
