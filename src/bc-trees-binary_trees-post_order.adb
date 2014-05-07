--  Copyright 1994 Grady Booch
--  Copyright 1998-2014 Simon Wright <simon@pushface.org>

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

procedure BC.Trees.Binary_Trees.Post_Order
  (T : Binary_Tree; Success : out Boolean) is
   Subtree : Binary_Tree;
   Result : Boolean;
begin
   Success := True;
   if not Is_Null (T) then
      Subtree := T;
      Left_Child (Subtree);
      Post_Order (Subtree, Result);
      if not Result then
         Success := False;
         return;
      end if;
      Subtree := T;
      Right_Child (Subtree);
      Post_Order (Subtree, Result);
      if not Result then
         Success := False;
         return;
      end if;
      Apply (T.Rep.Element, Result);
      if not Result then
         Success := False;
         return;
      end if;
   end if;
end BC.Trees.Binary_Trees.Post_Order;
