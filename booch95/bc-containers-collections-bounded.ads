--  Copyright (C) 1994-2002 Grady Booch, David Weller and Simon Wright.
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
package BC.Containers.Collections.Bounded is

   pragma Elaborate_Body;

   type Unconstrained_Collection
     (Maximum_Size : Positive) is new Abstract_Collection with private;

   subtype Collection
      is Unconstrained_Collection (Maximum_Size => Maximum_Size);

   function Null_Container return Unconstrained_Collection;
   --  Note, this function has to be provided but the object returned
   --  is in fact a Collection (ie, it is constrained).

   function "=" (Left, Right : in Unconstrained_Collection) return Boolean;

   procedure Clear (C : in out Unconstrained_Collection);
   --  Empty the collection of all items.

   procedure Insert (C : in out Unconstrained_Collection; Elem : Item);
   --  Add the item to the front of the collection.

   procedure Insert (C : in out Unconstrained_Collection;
                     Elem : Item;
                     Before : Positive);
   --  Add the item before the given index item in the collection; if
   --  before is 1, the item is added to the front of the collection.

   procedure Append (C : in out Unconstrained_Collection; Elem : Item);
   --  Add the item at the end of the collection.

   procedure Append (C : in out Unconstrained_Collection;
                     Elem : Item;
                     After : Positive);
   --  Add the item after the given index item in the collection.

   procedure Remove (C : in out Unconstrained_Collection; At_Index : Positive);
   --  Remove the item at the given index in the collection.

   procedure Replace (C : in out Unconstrained_Collection;
                      At_Index : Positive;
                      Elem : Item);
   --  Replace the item at the given index with the given item.

   function Available (C : in Unconstrained_Collection) return Natural;
   --  Indicated number of empty "Item slots" left in the collection.

   function Length (C : Unconstrained_Collection) return Natural;
   --  Return the number of items in the collection.

   function Is_Empty (C : Unconstrained_Collection) return Boolean;
   --  Return True if and only if there are no items in the
   --  collection.

   function First (C : Unconstrained_Collection) return Item;
   --  Return a copy of the item at the front of the collection.

   function Last (C : Unconstrained_Collection) return Item;
   --  Return a copy of the item at the end of the collection.

   function Item_At
     (C : Unconstrained_Collection; At_Index : Positive) return Item;
   --  Return a copy of the item at the indicated position in the
   --  collection.

   function Location
     (C : Unconstrained_Collection; Elem : Item) return Natural;
   --  Return the first index at which the item is found (0 if the
   --  item desn't exist in the collecton).

   function New_Iterator
     (For_The_Collection : Unconstrained_Collection) return Iterator'Class;
   --  Return a reset Iterator bound to the specific collection.

private

   function Item_At (C : Unconstrained_Collection;
                     Index : Positive) return Item_Ptr;

   package Collection_Nodes
   is new BC.Support.Bounded (Item => Item,
                              Item_Ptr => Item_Ptr);

   type Unconstrained_Collection
     (Maximum_Size : Positive)
   is new Abstract_Collection with record
      Rep : Collection_Nodes.Bnd_Node (Maximum_Size => Maximum_Size);
   end record;

end BC.Containers.Collections.Bounded;
