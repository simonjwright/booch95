-- Copyright (C) 1994-2001 Grady Booch and Simon Wright.
-- All Rights Reserved.
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

-- $Id$

generic
package BC.Containers.Collections is

  pragma Elaborate_Body;

  type Abstract_Collection is abstract new Container with private;

  -- A collection denotes an indexed collection of items, drawn from
  -- some well-defined universe. A collection may contain duplicate
  -- items; a collection owns a copy of each item.

  function Are_Equal (Left, Right : Abstract_Collection'Class) return Boolean;
  -- Return True if and only if both collections have the same extent
  -- and the same items in the same order; return False otherwise.
  -- Can't call this "=" because of the standard one for Collection.

  procedure Copy (From : Abstract_Collection'Class;
                  To : in out Abstract_Collection'Class);
  -- This operation MUST be called for dissimilar Collections in place of
  -- assignment.

  procedure Clear (C : in out Abstract_Collection) is abstract;
  -- Empty the collection of all items.

  procedure Insert (C : in out Abstract_Collection; Elem : Item) is abstract;
  -- Add the item to the front of the collection.

  procedure Insert (C : in out Abstract_Collection;
                    Elem : Item; Before : Positive) is abstract;
  -- Add the item before the given index item in the collection; if
  -- before is 1, the item is added to the front of the collection.

  procedure Append (C : in out Abstract_Collection; Elem : Item) is abstract;
  -- Add the item at the end of the collection.

  procedure Append (C : in out Abstract_Collection;
                    Elem : Item;
                    After : Positive) is abstract;
  -- Add the item after the given index item in the collection.

  procedure Remove (C : in out Abstract_Collection;
                    At_Index : Positive) is abstract;
  -- Remove the item at the given index in the collection.

  procedure Replace (C : in out Abstract_Collection;
                     At_Index : Positive; Elem : Item) is abstract;
  -- Replace the item at the given index with the given item.

  function Length (C : Abstract_Collection) return Natural is abstract;
  -- Return the number of items in the collection.

  function Is_Empty (C : Abstract_Collection) return Boolean is abstract;
  -- Return True if and only if there are no items in the collection.

  function First (C : Abstract_Collection) return Item is abstract;
  -- Return a copy of the item at the front of the collection.

  function Last (C : Abstract_Collection) return Item is abstract;
  -- Return a copy of the item at the end of the collection.

  function Item_At (C : Abstract_Collection;
                    At_Index : Positive) return Item is abstract;
  -- Return a copy of the item at the indicated position in the collection.

  function Location (C : Abstract_Collection;
                     Elem : Item) return Natural is abstract;
  -- Return the first index at which the item is found (0 if the
  -- item desn't exist in the collecton).

private

  type Abstract_Collection is abstract new Container with null record;

  procedure Lock (C : in out Abstract_Collection);

  procedure Unlock (C : in out Abstract_Collection);

  type Collection_Iterator is new Iterator with record
    Index : Natural;
  end record;

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Reset (It : in out Collection_Iterator);

  procedure Next (It : in out Collection_Iterator);

  function Is_Done (It : Collection_Iterator) return Boolean;

  function Current_Item (It : Collection_Iterator) return Item;

  function Current_Item_Ptr (It : Collection_Iterator) return Item_Ptr;

  procedure Delete_Item_At (It : in out Collection_Iterator);

end BC.Containers.Collections;
