--  Copyright 2004-2014 Simon Wright <simon@pushface.org>

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

generic
package BC.Trees.AVL_Trees.Iterators is

   type Iterator (<>) is private;

   function New_Iterator (For_The_Container : AVL_Tree) return Iterator;

   procedure Reset (It : in out Iterator);

   function Is_Done (It : Iterator) return Boolean;

   function Current_Item (It : Iterator) return Item;

   procedure Next (It : in out Iterator);

private

   type Container_Ptr is access all AVL_Tree;
   for Container_Ptr'Storage_Size use 0;

   type Iterator is record
      For_The_Container : Container_Ptr;
      Previous, Current : AVL_Node_Ref;
   end record;

end BC.Trees.AVL_Trees.Iterators;
