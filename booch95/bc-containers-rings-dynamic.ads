--  Copyright (C) 1994-2001 Grady Booch and Simon Wright.
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

--  $Id$

with BC.Support.Dynamic;
with System.Storage_Pools;

generic
   type Storage_Manager (<>)
   is new System.Storage_Pools.Root_Storage_Pool with private;
   Storage : in out Storage_Manager;
   Initial_Size : Positive := 10;
package BC.Containers.Rings.Dynamic is

   pragma Elaborate_Body;

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
   --  If the ring is still not empty, the new top is the item that
   --  was previously one place forward from the top.
   --  If the removed item was the marked item, the mark now
   --  designates the new top.

   procedure Rotate (R : in out Ring; Dir : Direction := Forward);
   --  Rotate the top of the ring in the given direction. The ring's
   --  mark is unaffected. If there is exactly one item in the ring,
   --  rotating either direction always returns to the same item.

   function Extent (R : Ring) return Natural;
   --  Return the number of items in the ring.

   function Is_Empty (R : Ring) return Boolean;
   --  Return True if and only if there are no items in the ring.

   function Top (R : Ring) return Item;
   --  Return a copy of the item at the top of the ring.

   procedure Preallocate (R : in out Ring; Size : Natural);
   --  Allocates 'Size' additional storage elements for the Ring

   procedure Set_Chunk_Size (R : in out Ring; Size : Natural);
   --  Establishes the Size the Ring will grow if the Ring exhausts
   --  its current size.

   function Chunk_Size (R : Ring) return Natural;
   --  Returns the Chunk_Size

   function New_Iterator (For_The_Ring : Ring) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Ring.

private

   procedure Add (R : in out Ring; Elem : Item);
   function Item_At (R : Ring; Index : Positive) return Item_Ptr;

   package Ring_Nodes
   is new BC.Support.Dynamic (Item => Item,
                              Item_Ptr => Item_Ptr,
                              Storage_Manager => Storage_Manager,
                              Storage => Storage,
                              Initial_Size => Initial_Size);

   type Ring is new Abstract_Ring with record
      Rep : Ring_Nodes.Dyn_Node;
   end record;

end BC.Containers.Rings.Dynamic;
