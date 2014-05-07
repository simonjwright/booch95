--  Copyright 2001-2014 Simon Wright <simon@pushface.org>

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

--  Algorithm from "Algorithms", Robert Sedgewick, Addison-Wesley 1983

procedure BC.Containers.Shellsort (C : in out Container) is
   procedure Sort (C : in out Containers.Container'Class);
   pragma Inline (Sort);
   Len : constant Natural := Length (C);
   procedure Sort (C : in out Containers.Container'Class) is
      H : Positive := 1;
      J : Positive;
      V : Item;
   begin
      loop
         H := 3 * H + 1;
         exit when H > Len;
      end loop;
      loop
         H := H / 3;
         for I in H + 1 .. Len loop
            V := Item_At (C, I).all;
            J := I;
            while V < Item_At (C, J - H).all loop
               Item_At (C, J).all := Item_At (C, J - H).all;
               J := J - H;
               exit when J <= H;
            end loop;
            Item_At (C, J).all := V;
         end loop;
         exit when H = 1;
      end loop;
   end Sort;
begin
   Sort (C);
exception
   when Should_Have_Been_Overridden => raise Sort_Error;
end BC.Containers.Shellsort;
