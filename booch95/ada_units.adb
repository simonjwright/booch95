--  Copyright (C) 1998,2001 Simon Wright.
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
with Ada_Unit_Support;

procedure Ada_Units is
   use Ada_Unit_Support;
   U_Text_IO : Unit_P := Create_Normal_Unit ("ada.text_io");
   U_Ada_Units : Unit_P := Create_Normal_Unit ("ada_units");
   U_Ada_Unit_Support : Unit_P := Create_Normal_Unit ("ada_unit_support");
   U_Directed : Unit_P := Create_Generic_Unit ("bc.graphs.directed");
   U_Undirected : Unit_P := Create_Generic_Unit ("bc.graphs.undirected");
   U_Graphs : Unit_P := Create_Generic_Unit ("bc.graphs");
begin
   Add_Dependency (U_Graphs, U_Ada_Unit_Support);
   Add_Dependency (U_Directed, U_Ada_Unit_Support);
   Add_Dependency (U_Undirected, U_Ada_Unit_Support);
   Add_Dependency (U_Text_IO, U_Ada_Units);
   Add_Dependency (U_Ada_Unit_Support, U_Ada_Units);
   Add_Dependency (U_Text_IO, U_Ada_Unit_Support);
   Report_Dependencies;
   Ada.Text_IO.New_Line;
   Report_Dependencies (U_Ada_Units);
   Ada.Text_IO.New_Line;
   Report_Dependencies (U_Ada_Unit_Support);
end Ada_Units;
