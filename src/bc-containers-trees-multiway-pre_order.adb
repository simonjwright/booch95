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

procedure BC.Containers.Trees.Multiway.Pre_Order
  (T : Multiway_Tree; Success : out Boolean) is
   Result : Boolean;
begin
   Success := True;
   if not Is_Null (T) then
      Apply (T.Rep.Element, Result);
      if not Result then
         Success := False;
         return;
      end if;
      for I in 1 .. Arity (T) loop
         declare
            Subtree : Multiway_Tree := T;
         begin
            Child (Subtree, I);
            Pre_Order (Subtree, Result);
            if not Result then
               Success := False;
               return;
            end if;
         end;
      end loop;
   end if;
end BC.Containers.Trees.Multiway.Pre_Order;
