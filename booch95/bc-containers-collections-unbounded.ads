-- Copyright (C) 1994-2000 Grady Booch and Simon Wright.
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

with BC.Support.Unbounded;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Collections.Unbounded is

  pragma Elaborate_Body;

  type Unbounded_Collection is new Collection with private;

  function "=" (Left, Right : in Unbounded_Collection) return Boolean;

  procedure Clear (C : in out Unbounded_Collection);
  -- Empty the collection of all items.

  procedure Insert (C : in out Unbounded_Collection; Elem : Item);
  -- Add the item to the front of the collection.

  procedure Insert (C : in out Unbounded_Collection;
                    Elem : Item;
                    Before : Positive);
  -- Add the item before the given index item in the collection; if
  -- before is 1, the item is added to the front of the collection.

  procedure Append (C : in out Unbounded_Collection; Elem : Item);
  -- Add the item at the end of the collection.

  procedure Append (C : in out Unbounded_Collection;
                    Elem : Item;
                    After : Positive);
  -- Add the item after the given index item in the collection.

  procedure Remove (C : in out Unbounded_Collection; At_Index : Positive);
  -- Remove the item at the given index in the collection.

  procedure Replace (C : in out Unbounded_Collection;
                     At_Index : Positive;
                     Elem : Item);
  -- Replace the item at the given index with the given item.

  function Length (C : Unbounded_Collection) return Natural;
  -- Return the number of items in the collection.

  function Is_Empty (C : Unbounded_Collection) return Boolean;
  -- Return True if and only if there are no items in the collection.

  function First (C : Unbounded_Collection) return Item;
  -- Return a copy of the item at the front of the collection.

  function Last (C : Unbounded_Collection) return Item;
  -- Return a copy of the item at the end of the collection.

  function Item_At
     (C : Unbounded_Collection; At_Index : Positive) return Item;
  -- Return a copy of the item at the indicated position in the collection.

  function Location (C : Unbounded_Collection; Elem : Item) return Natural;
  -- Return the first index at which the item is found (0 if the
  -- item desn't exist in the collecton).

  function New_Iterator
     (For_The_Collection : Unbounded_Collection) return Iterator'Class;
  -- Return a reset Iterator bound to the specific Collection.

private

  function Item_At (C : Unbounded_Collection; Index : Positive) return Item_Ptr;

  package Unbounded_Collection_Nodes
  is new BC.Support.Unbounded (Item => Item,
                               Item_Ptr => Item_Ptr,
                               Storage_Manager => Storage_Manager,
                               Storage => Storage);

  type Unbounded_Collection is new Collection with record
    Rep : Unbounded_Collection_Nodes.Unb_Node_Ref
       := new Unbounded_Collection_Nodes.Unb_Node;
  end record;

  procedure Initialize (C : in out Unbounded_Collection);
  procedure Adjust (C : in out Unbounded_Collection);
  procedure Finalize (C : in out Unbounded_Collection);

end BC.Containers.Collections.Unbounded;
