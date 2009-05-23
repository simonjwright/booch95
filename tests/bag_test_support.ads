--  Copyright 1994 Grady Booch
--  Copyright 1998-2009 Simon Wright <simon@pushface.org>

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

--  $Revision$
--  $Date$
--  $Author$

with BC.Containers;
with BC.Containers.Bags;
with BC.Containers.Bags.Bounded;
with BC.Containers.Bags.Dynamic;
with BC.Containers.Bags.Unbounded;
with BC.Containers.Bags.Unmanaged;
with BC.Support.Standard_Storage;
with Global_Heap;

package Bag_Test_Support is

   package Containers is new BC.Containers (Item => Character);

   package Bags is new Containers.Bags;

   function Char_Hash (C : Character) return Natural;

   package BB is new Bags.Bounded
     (Hash => Char_Hash,
      Buckets => 3,
      Maximum_Size => 100);

   package BD is new Bags.Dynamic
     (Hash => Char_Hash,
      Buckets => 3,
      Storage => Global_Heap.Storage);

   package BU is new Bags.Unbounded
     (Hash => Char_Hash,
      Buckets => 3,
      Storage => BC.Support.Standard_Storage.Pool);

   package BUM is new Bags.unmanaged
     (Hash => Char_Hash,
      Buckets => 3);

end Bag_Test_Support;
