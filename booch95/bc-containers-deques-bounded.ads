--  Copyright (C) 1994-2001 Grady Booch, David Weller and Simon Wright.
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

with BC.Support.Bounded;

generic
   Maximum_Size : Positive;
package BC.Containers.Deques.Bounded is

   pragma Elaborate_Body;

   type Deque is new Abstract_Deque with private;

   function Null_Container return Deque;

   procedure Clear (D : in out Deque);
   --  Empty the deque of all items.

   procedure Append (D : in out Deque;
                     Elem : Item;
                     Location : Deque_End := Back);
   --  Add the item to the deque at the given location; the item
   --  itself is copied.

   procedure Pop (D : in out Deque; Location : Deque_End := Front);
   --  Remove the item from the deque at the given location.

   procedure Remove (D : in out Deque; From : Positive);
   --  Remove the item at the given index.

   function Available (D : in Deque) return Natural;
   --  Indicates number of empty "Item slots" left in Deque

   function Length (D : in Deque) return Natural;
   --  Return the number of items in the deque.

   function Is_Empty (D : in Deque) return Boolean;
   --  Return True if and only if there are no items in the deque.

   function Front (D : in Deque) return Item;
   --  Return a copy of the item at the front of the deque.

   function Back (D : in Deque) return Item;
   --  Return a copy of the item at the back of the deque.

   function Location (D : in Deque; Elem : Item) return Natural;
   --  Return the first index at which the item is found; return 0 if
   --  the item does not exist in the deque.

   function "=" (Left, Right : in Deque) return Boolean;
   --  Return True if and only if both deques have the same length and
   --  the same items in the same order; return False otherwise.

   function New_Iterator (For_The_Deque : Deque) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Deque.

private

   package Deque_Nodes
   is new BC.Support.Bounded (Item => Item,
                              Item_Ptr => Item_Ptr,
                              Maximum_Size => Maximum_Size);

   type Deque is new Abstract_Deque with record
      Rep : Deque_Nodes.Bnd_Node;
   end record;

   function Item_At (D : Deque; Index : Positive) return Item_Ptr;

end BC.Containers.Deques.Bounded;
