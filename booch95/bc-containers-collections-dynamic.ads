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

with BC.Support.Dynamic;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
  Initial_Size : Positive := 10;
package BC.Containers.Collections.Dynamic is

  pragma Elaborate_Body;

  type Collection is new Abstract_Collection with private;

  function Null_Container return Collection;

  function "=" (Left, Right : in Collection) return Boolean;

  procedure Clear (C : in out Collection);
  -- Empty the collection of all items.

  procedure Insert (C : in out Collection; Elem : Item);
  -- Add the item to the front of the collection.

  procedure Insert (C : in out Collection;
                    Elem : Item;
                    Before : Positive);
  -- Add the item before the given index item in the collection; if
  -- before is 1, the item is added to the front of the collection.

  procedure Append (C : in out Collection; Elem : Item);
  -- Add the item at the end of the collection.

  procedure Append (C : in out Collection;
                    Elem : Item;
                    After : Positive);
  -- Add the item after the given index item in the collection.

  procedure Remove (C : in out Collection; At_Index : Positive);
  -- Remove the item at the given index in the collection.

  procedure Replace (C : in out Collection;
                     At_Index : Positive;
                     Elem : Item);
  -- Replace the item at the given index with the given item.

  function Length (C : Collection) return Natural;
  -- Return the number of items in the collection.

  function Is_Empty (C : Collection) return Boolean;
  -- Return True if and only if there are no items in the collection.

  function First (C : Collection) return Item;
  -- Return a copy of the item at the front of the collection.

  function Last (C : Collection) return Item;
  -- Return a copy of the item at the end of the collection.

  function Item_At
     (C : Collection; At_Index : Positive) return Item;
  -- Return a copy of the item at the indicated position in the collection.

  function Location (C : Collection; Elem : Item) return Natural;
  -- Return the first index at which the item is found (0 if the
  -- item desn't exist in the collecton).

  procedure Preallocate (C : in out Collection; Size : Natural);
  -- Allocates 'Size' additional storage elements for the Collection

  procedure Set_Chunk_Size (C : in out Collection; Size : Natural);
  -- Establishes the Size the Collection will grow if the Collection
  -- exhausts its current size.

  function Chunk_Size (C : Collection) return Natural;
  -- Returns the Chunk_Size

  function New_Iterator
     (For_The_Collection : Collection) return Iterator'Class;
  -- Return a reset Iterator bound to the specific Collection.

private

  function Item_At (C : Collection; Index : Positive) return Item_Ptr;

  package Collection_Nodes
  is new BC.Support.Dynamic (Item => Item,
                             Item_Ptr => Item_Ptr,
                             Storage_Manager => Storage_Manager,
                             Storage => Storage);

  type Collection is new Abstract_Collection with record
    Rep : Collection_Nodes.Dyn_Node_Ref;
  end record;

  procedure Initialize (C : in out Collection);
  procedure Adjust (C : in out Collection);
  procedure Finalize (C : in out Collection);

end BC.Containers.Collections.Dynamic;
