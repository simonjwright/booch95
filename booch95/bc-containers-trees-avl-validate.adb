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
procedure BC.Containers.Trees.AVL.Validate (T : AVL_Tree) is
  use Ada.Text_Io;
  use type Nodes.Avl_Node_Ref;
  use type Nodes.Node_Balance;
  function Depth (N : Nodes.Avl_Node_Ref) return Natural is
  begin
    if N = null then
      return 0;
    else
      return 1 + Natural'Max (Depth (N.Left), Depth (N.Right));
    end if;
  end Depth;
  procedure Validate (N : Nodes.AVL_Node_Ref) is
    Left_Depth : constant Natural := Depth (N.Left);
    Right_Depth : constant Natural := Depth (N.Right);
  begin
    if N.Left /= null then
      Validate (N.Left);
    end if;
    if N.Right /= null then
      Validate (N.Right);
    end if;
    if Left_Depth = Right_Depth then
      if N.Balance /= Nodes.Middle then
        Put_Line ("depths equal but balance "
                  & Nodes.Node_Balance'Image (N.Balance));
      end if;
    elsif Left_Depth > Right_Depth then
      if Left_Depth - Right_Depth /= 1 then
        Put_Line ("left depth is"
                  & Natural'Image (Left_Depth - Right_Depth)
                  & " greater than right depth");
      end if;
      if N.Balance /= Nodes.Left then
        Put_Line ("left deeper than right but balance "
                  & Nodes.Node_Balance'Image (N.Balance));
      end if;
    else
      if Right_Depth - Left_Depth /= 1 then
        Put_Line ("right depth is"
                  & Natural'Image (Right_Depth - Left_Depth)
                  & " greater than left depth");
      end if;
      if N.Balance /= Nodes.Right then
        Put_Line ("right deeper than left but balance "
                  & Nodes.Node_Balance'Image (N.Balance));
      end if;
    end if;
  end Validate;
begin
  if T.Rep /= null then
    Validate (T.Rep);
  end if;
end BC.Containers.Trees.AVL.Validate;
