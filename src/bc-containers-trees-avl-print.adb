--  Copyright 1999-2014 Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

with Ada.Text_IO;
procedure BC.Containers.Trees.AVL.Print (T : AVL_Tree) is
   use Ada.Text_IO;
   procedure Print_Node (N : Support.AVL_Node_Ref; Indent : Natural);
   procedure Print_Node (N : Support.AVL_Node_Ref; Indent : Natural) is
      use type Support.AVL_Node_Ref;
   begin
      if N.Left /= null then
         Print_Node (N.Left, Indent + 1);
      end if;
      for I in 1 .. Indent loop
         Put ("  ");
      end loop;
      Put ("element: " & Image (N.Element));
      Put (" (" & Support.Node_Balance'Image (N.Balance) & ")");
      New_Line;
      if N.Right /= null then
         Print_Node (N.Right, Indent + 1);
      end if;
   end Print_Node;
begin
   Put_Line ("tree of size" & Natural'Image (T.Rep.Size));
   Print_Node (T.Rep.Rep, 0);
end BC.Containers.Trees.AVL.Print;
