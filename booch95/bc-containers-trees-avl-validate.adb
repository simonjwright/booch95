--  Copyright (C) 1999, 2001-2002 Simon Wright.
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

with Ada.Text_IO;
procedure BC.Containers.Trees.AVL.Validate (T : AVL_Tree) is
   use Ada.Text_IO;

   Overall_Depth : Natural;

   function Validate (N : AVL_Node_Ref) return Natural;
   function Validate (N : AVL_Node_Ref) return Natural is

      Left_Depth : Natural;
      Right_Depth : Natural;

   begin

      if N = null then
         return 0;
      end if;

      Left_Depth := Validate (N.Left);
      Right_Depth := Validate (N.Right);

      if Left_Depth = Right_Depth then
         if N.Balance /= Middle then
            Put_Line ("depths equal but balance "
                        & Node_Balance'Image (N.Balance));
         end if;
      elsif Left_Depth > Right_Depth then
         if Left_Depth - Right_Depth /= 1 then
            Put_Line ("left depth is"
                        & Natural'Image (Left_Depth - Right_Depth)
                        & " greater than right depth");
         end if;
         if N.Balance /= Left then
            Put_Line ("left deeper than right but balance "
                        & Node_Balance'Image (N.Balance));
         end if;
      else
         if Right_Depth - Left_Depth /= 1 then
            Put_Line ("right depth is"
                        & Natural'Image (Right_Depth - Left_Depth)
                        & " greater than left depth");
         end if;
         if N.Balance /= Right then
            Put_Line ("right deeper than left but balance "
                        & Node_Balance'Image (N.Balance));
         end if;
      end if;

      return 1 + Natural'Max (Left_Depth, Right_Depth);

   end Validate;

begin
   Overall_Depth :=  Validate (T.Rep);
end BC.Containers.Trees.AVL.Validate;
