-- Copyright (C) 1999 Simon Wright.
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
procedure BC.Containers.Trees.AVL.Print (T : AVL_Tree) is
  use Ada.Text_Io;
  procedure Print_Node (N : Nodes.AVL_Node_Ref; Indent : Natural) is
    use type Nodes.AVL_Node_Ref;
    use type Nodes.Node_Balance;
  begin
    if N.Left /= null then
      Print_Node (N.Left, Indent + 1);
    end if;
    for I in 1 .. Indent loop
      Put ("  ");
    end loop;
    Put ("element: " & Image (N.Element));
    Put (" (" & Nodes.Node_Balance'Image (N.Balance) & ")");
    New_Line;
    if N.Right /= null then
      Print_Node (N.Right, Indent + 1);
    end if;
  end Print_Node;
begin
  Put_Line ("tree of size" & Natural'Image (T.Size));
  Print_Node (T.Rep, 0);
end BC.Containers.Trees.AVL.Print;
