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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with BC.Support.Bounded;

generic
   Maximum_Size : Positive;
package BC.Containers.Collections.Ordered.Bounded is

   pragma Elaborate_Body;

   type Collection is new Abstract_Ordered_Collection with private;

   function Null_Container return Collection;

   function "=" (Left, Right : in Collection) return Boolean;

   procedure Clear (C : in out Collection);
   --  Empty the collection of all items.

   procedure Insert (C : in out Collection; Elem : Item);
   --  Add the item to the collection, starting at the front.

   procedure Insert (C : in out Collection;
                     Elem : Item;
                     Before : Positive);
   --  Add the item to the collection, starting at the front.

   procedure Append (C : in out Collection; Elem : Item);
   --  Add the item to the collection, starting at the end.

   procedure Append (C : in out Collection;
                     Elem : Item;
                     After : Positive);
   --  Add the item to the collection, starting at the end.

   procedure Remove (C : in out Collection; At_Index : Positive);
   --  Remove the item at the given index in the collection.

   procedure Replace (C : in out Collection;
                      At_Index : Positive;
                      Elem : Item);
   --  Replace the item at the given index with the given item.

   function Available (C : in Collection) return Natural;
   --  Indicated number of empty "Item slots" left in Collection

   function Length (C : Collection) return Natural;
   --  Return the number of items in the collection.

   function Is_Empty (C : Collection) return Boolean;
   --  Return True if and only if there are no items in the collection.

   function First (C : Collection) return Item;
   --  Return a copy of the item at the front of the collection.

   function Last (C : Collection) return Item;
   --  Return a copy of the item at the end of the collection.

   function Item_At (C : Collection; At_Index : Positive) return Item;
   --  Return a copy of the item at the indicated position in the collection.

   function Location (C : Collection; Elem : Item) return Natural;
   --  Return the first index at which the item is found (0 if the
   --  item desn't exist in the collecton).

   function New_Iterator
     (For_The_Collection : Collection) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Collection.

private

   function Item_At
     (C : Collection; Index : Positive) return Item_Ptr;

   package Collection_Nodes
   is new BC.Support.Bounded (Item => Item,
                              Item_Ptr => Item_Ptr,
                              Maximum_Size => Maximum_Size);

   type Collection is new Abstract_Ordered_Collection with record
      Rep : Collection_Nodes.Bnd_Node;
   end record;

end BC.Containers.Collections.Ordered.Bounded;
