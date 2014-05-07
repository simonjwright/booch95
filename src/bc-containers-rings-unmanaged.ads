--  Copyright 1994 Grady Booch
--  Copyright 2003-2014 Simon Wright <simon@pushface.org>

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

with BC.Support.Unmanaged;

generic
package BC.Containers.Rings.Unmanaged is

   pragma Preelaborate;

   type Ring is new Abstract_Ring with private;

   function Null_Container return Ring;

   function "=" (Left, Right : in Ring) return Boolean;

   procedure Clear (R : in out Ring);
   --  Empty the ring of all items. The mark is cleared.

   procedure Insert (R : in out Ring; Elem : Item);
   --  If the ring was empty, set the ring's mark and top to designate
   --  this item.
   --  Otherwise,
   --    this item becomes the new top;
   --    the previous top is located one place forward of the new top;
   --    the mark remains on the previously marked item.

   procedure Pop (R : in out Ring);
   --  Remove the top item from the ring.
   --  If the ring is still not empty, the new top is the item that was
   --  previously one place forward from the top.
   --  If the removed item was the marked item, the mark now designates
   --  the new top.

   function Extent (R : Ring) return Natural;
   --  Return the number of items in the ring.

   function Is_Empty (R : Ring) return Boolean;
   --  Return True if and only if there are no items in the ring.

   function Top (R : Ring) return Item;
   --  Return a copy of the item at the top of the ring.

   function New_Iterator (For_The_Ring : Ring) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Ring.

private

   procedure Add (R : in out Ring; Elem : Item);
   function Item_At (R : Ring; Index : Positive) return Item_Ptr;

   package Ring_Nodes
   is new BC.Support.Unmanaged (Item => Item,
                                Item_Ptr => Item_Ptr);

   type Ring is new Abstract_Ring with record
      Rep : Ring_Nodes.Unm_Node;
   end record;

end BC.Containers.Rings.Unmanaged;
