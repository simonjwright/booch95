--  Copyright (C) 2001 Simon Wright.
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

with BC.Containers.Collections.Unbounded;
with BC.Support.Standard_Storage;

generic
   type Item is private;
   with function "=" (L, R : Item) return Boolean is <>;
package BC.Simple_Collections is

   pragma Elaborate_Body;

   package Abstract_Containers is new BC.Containers (Item);
   package Abstract_Collections is new Abstract_Containers.Collections;
   package Collections is new Abstract_Collections.Unbounded
     (Storage => BC.Support.Standard_Storage.Pool);


   type Collection is new Collections.Collection with private;
   --  A collection denotes an indexed collection of items, drawn from
   --  some well-defined universe. A collection may contain duplicate
   --  items; a collection owns a copy of each item.


   -----------------------------
   --  Collection operations  --
   -----------------------------

   function Null_Collection return Collection;
   --  Return an empty Collection.

   function "=" (Left, Right : in Collection) return Boolean;

   procedure Clear (C : in out Collection);
   --  Empty the collection of all items.

   procedure Insert (C : in out Collection; Elem : Item);
   --  Add the item to the front of the collection.

   procedure Insert (C : in out Collection;
                     Elem : Item; Before : Positive);
   --  Add the item before the given index item in the collection; if
   --  before is 1, the item is added to the front of the collection.

   procedure Append (C : in out Collection; Elem : Item);
   --  Add the item at the end of the collection.

   procedure Append (C : in out Collection;
                     Elem : Item;
                     After : Positive);
   --  Add the item after the given index item in the collection.

   procedure Remove (C : in out Collection;
                     At_Index : Positive);
   --  Remove the item at the given index in the collection.

   procedure Replace (C : in out Collection;
                      At_Index : Positive; Elem : Item);
   --  Replace the item at the given index with the given item.

   function Length (C : Collection) return Natural;
   --  Return the number of items in the collection.

   function Is_Empty (C : Collection) return Boolean;
   --  Return True if and only if there are no items in the
   --  collection.

   function First (C : Collection) return Item;
   --  Return a copy of the item at the front of the collection.

   function Last (C : Collection) return Item;
   --  Return a copy of the item at the end of the collection.

   function Item_At (C : Collection;
                     At_Index : Positive) return Item;
   --  Return a copy of the item at the indicated position in the
   --  collection.

   function Location (C : Collection;
                      Elem : Item) return Natural;
   --  Return the first index at which the item is found (0 if the
   --  item desn't exist in the collection).


   -----------------
   --  Iteration  --
   -----------------

   subtype Iterator is Abstract_Containers.Iterator'Class;

   function New_Iterator
     (For_The_Collection : Collection) return Iterator;
   --  Return a reset Iterator bound to the specific Collection.

   procedure Reset (It : in out Iterator);
   --  Reset the Iterator to the beginning.

   procedure Next (It : in out Iterator);
   --  Advance the Iterator to the next Item in the Container.

   function Is_Done (It : Iterator) return Boolean;
   --  Return True if there are no more Items in the Container.

   function Current_Item (It : Iterator) return Item;
   --  Return a copy of the current Item.


private

   type Collection is new Collections.Collection with null record;

   function Null_Container return Collection;

end BC.Simple_Collections;
