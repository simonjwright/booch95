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
package BC.Containers.Queues.Ordered.Bounded is

   pragma Elaborate_Body;

   type Queue is new Abstract_Ordered_Queue with private;

   function Null_Container return Queue;

   procedure Clear (Q : in out Queue);
   --  Empty the queue of all items.

   procedure Append (Q : in out Queue; Elem : Item);
   --  Add the item to the queue; the item itself is copied.

   procedure Pop (Q : in out Queue);
   --  Remove the item from the front of the queue.

   procedure Remove (Q : in out Queue; From : Positive);
   --  Remove the item at the given index.

   function Available (Q : in Queue) return Natural;
   --  Indicates number of empty "Item slots" left in Queue

   function Length (Q : in Queue) return Natural;
   --  Return the number of items in the queue.

   function Is_Empty (Q : in Queue) return Boolean;
   --  Return True if and only if there are no items in the queue.

   function Front (Q : in Queue) return Item;
   --  Return a copy of the item at the front of the queue.

   function Location (Q : in Queue; Elem : Item) return Natural;
   --  Return the first index at which the item is found; return 0 if
   --  the item does not exist in the queue.

   function "=" (Left, Right : in Queue) return Boolean;
   --  Return True if and only if both queues have the same length and
   --  the same items; return False otherwise.

   function New_Iterator (For_The_Queue : Queue) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Queue.

private

   package Queue_Nodes
   is new BC.Support.Bounded (Item => Item,
                              Item_Ptr => Item_Ptr,
                              Maximum_Size => Maximum_Size);

   type Queue is new Abstract_Ordered_Queue with record
      Rep : Queue_Nodes.Bnd_Node;
   end record;

   function Item_At (Q : Queue; Index : Positive) return Item_Ptr;

end BC.Containers.Queues.Ordered.Bounded;
