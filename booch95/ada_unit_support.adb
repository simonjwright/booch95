--  Copyright (C) 1998-2001 Simon Wright.
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

--  $Id$

with Ada.Text_IO;

package body Ada_Unit_Support is


   package ASU renames Ada.Strings.Unbounded;


   Info : Dependencies.Graph;


   function Name (Of_The_Unit : Unit) return String is
   begin
      return ASU.To_String (Of_The_Unit.Named);
   end Name;


   function Description (Of_The_Unit : Normal_Unit) return String is
   begin
      return "normal";
   end Description;


   function Description (Of_The_Unit : Generic_Unit) return String is
   begin
      return "generic";
   end Description;


   function Create_Normal_Unit (Unit_Named : String) return Unit_P is
      Result : Unit_P := new Normal_Unit;
   begin
      Result.Named := ASU.To_Unbounded_String (Unit_Named);
      Dependencies.Create_Vertex (G => Info,
                                  V => Result.Vertex,
                                  I => Result);
      return Result;
   end Create_Normal_Unit;


   function Create_Generic_Unit (Unit_Named : String) return Unit_P is
      Result : Unit_P := new Generic_Unit;
   begin
      Result.Named := ASU.To_Unbounded_String (Unit_Named);
      Dependencies.Create_Vertex (G => Info,
                                  V => Result.Vertex,
                                  I => Result);
      return Result;
   end Create_Generic_Unit;


   procedure Add_Dependency (Unit_Is_Withed : Unit_P; By : Unit_P) is
      Arc : Dependencies.Arc;
   begin
      Dependencies.Create_Arc (G => Info,
                               A => Arc,
                               I => (null record),
                               From => By.Vertex,
                               To => Unit_Is_Withed.Vertex);
   end Add_Dependency;


   procedure Report_Dependencies is
      procedure Process_Dependency
        (A : Dependencies_Base.Abstract_Arc'Class; OK : out Boolean);
      procedure Process_Dependency
        (A : Dependencies_Base.Abstract_Arc'Class; OK : out Boolean) is
         Withee : Dependencies.Vertex;
      begin
         Dependencies.To_Vertex (Dependencies.Arc (A), Withee);
         Ada.Text_IO.Put_Line ("  withs "
                               & Name (Dependencies.Item (Withee).all));
         OK := True;
      end Process_Dependency;
      procedure Process_Unit
        (V : Dependencies_Base.Abstract_Vertex'Class; OK : out Boolean);
      procedure Process_Unit
        (V : Dependencies_Base.Abstract_Vertex'Class; OK : out Boolean) is
         procedure Visit
         is new Dependencies_Base.Visit_Arcs (Apply => Process_Dependency);
         Directed_V : Dependencies.Vertex renames Dependencies.Vertex (V);
         Vertex_It : Dependencies_Base.Vertex_Iterator'Class
           := Dependencies.New_Vertex_Outgoing_Iterator (Directed_V);
      begin
         Ada.Text_IO.Put_Line (Description (Dependencies.Item (Directed_V).all)
                               & " unit "
                               & Name (Dependencies.Item (Directed_V).all));
         Visit (Using => Vertex_It);
         OK := True;
      end Process_Unit;
      procedure Visit is new Dependencies_Base.Visit_Vertices
        (Apply => Process_Unit);
      Graph_It : Dependencies_Base.Graph_Iterator'Class
        := Dependencies.New_Graph_Iterator (Info);
   begin
      Visit (Using => Graph_It);
   end Report_Dependencies;


   procedure Report_Dependencies (For_Unit : Unit_P) is
      Indent : ASU.Unbounded_String;
      procedure Process_Unit
        (V : Dependencies_Base.Abstract_Vertex'Class; OK : out Boolean);
      procedure Process_Unit
        (V : Dependencies_Base.Abstract_Vertex'Class; OK : out Boolean) is
         procedure Process_Dependency
           (A : Dependencies_Base.Abstract_Arc'Class; OK : out Boolean);
         procedure Process_Dependency
           (A : Dependencies_Base.Abstract_Arc'Class; OK : out Boolean) is
            Withee : Dependencies.Vertex;
         begin
            Dependencies.To_Vertex (Dependencies.Arc (A), Withee);
            Process_Unit (Withee, OK);
         end Process_Dependency;
         procedure Visit
         is new Dependencies_Base.Visit_Arcs (Apply => Process_Dependency);
         Directed_V : Dependencies.Vertex renames Dependencies.Vertex (V);
         Vertex_It : Dependencies_Base.Vertex_Iterator'Class
           := Dependencies.New_Vertex_Outgoing_Iterator (Directed_V);
         Old_Indent : ASU.Unbounded_String := Indent;
         use type ASU.Unbounded_String;
      begin
         Ada.Text_IO.Put_Line
           (ASU.To_String (Indent)
            & Description (Dependencies.Item (Directed_V).all)
            & " unit "
            & Name (Dependencies.Item (Directed_V).all));
         Indent := Indent & "  ";
         Visit (Vertex_It);
         Indent := Old_Indent;
         OK := True;
      end Process_Unit;
      Dummy : Boolean;
   begin
      Process_Unit (For_Unit.Vertex, Dummy);
   end Report_Dependencies;


end Ada_Unit_Support;
