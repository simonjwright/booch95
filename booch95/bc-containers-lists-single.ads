-- Copyright (C) 1994-1998 Grady Booch, David Weller and Simon Wright.
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

with BC.Support.Nodes;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Containers.Lists.Single is

  -- Singly-linked list

  type Single_List is new Container with private;

  function "=" (L, R : Single_List) return Boolean;
  -- Return True if and only if both lists are null or structurally share
  -- the same list.

  procedure Clear (Obj : in out Single_List);
  -- If the list is not null, destroy this alias to the list, make the list
  -- null, and reclaim the storage associated with any unreachable items.

  procedure Insert (Obj : in out Single_List; Elem : Item);
  -- Add the item to the head of the list.

  procedure Insert (Obj : in out Single_List; From_List : in Single_List);
  -- Add the given list to the head of the list.

  procedure Insert (Obj : in out Single_List; Elem : Item; Before : Positive);
  -- Add the item before the given index item in the list; if before is 1,
  -- the item is added to the head of the list.

  procedure Insert (Obj : in out Single_List;
                    From_List: in out Single_List;
                    Before : Positive);
  -- Add the list before the given index item in the list; if before is 1,
  -- the list is added to the head of the list.

  procedure Append (Obj : in out Single_List; Elem : Item);
  -- Add the item at the end of the list.

  procedure Append (Obj : in out Single_List; From_List : in Single_List);
  -- Add the given list at the end of the list.

  procedure Append (Obj : in out Single_List; Elem : Item; After : Positive);
  -- Add the item after the given index item in the list.

  procedure Append (Obj : in out Single_List;
                    From_List : in Single_List;
                    After : Positive);
  -- Add the list after the given index item in the list.

  procedure Remove (Obj : in out Single_List; From : Positive);
  -- Remove the item at the given index in the list.

  procedure Purge (Obj : in out Single_List; From : Positive);
  -- Remove all the items in the list starting at the given index,
  -- inclusive.

  procedure Purge (Obj : in out Single_List; From : Positive; Count : Positive);
  -- Remove all the items in the list starting at the given index,
  -- inclusive, for a total of count items.

  procedure Preserve (Obj : in out Single_List; From : Positive);
  -- Remove all the items in the list except those starting at the given
  -- index, inclusive.

  procedure Preserve (Obj : in out Single_List;
                      From : Positive;
                      Count : Positive);
  -- Remove all the items in the list except those starting at the given
  -- index, inclusive, for a total of count items.

  procedure Share (Obj : in out Single_List;
                   With_List: Single_List;
                   Starting_At : Positive);
  -- Clear the list, then, if the given list is not null, set the list to
  -- structurally share with the head of the given list, starting at the
  -- given index.

  procedure Share_Head (Obj : in out Single_List; With_List : in Single_List);
  -- Clear the list, then, if the given list is not null, set the list to
  -- structurally share with the head of the given list.

  procedure Share_Foot (Obj : in out Single_List; With_List : in Single_List);
  -- Clear the list, then, if the given list is not null, set the list to
  -- structurally share with the end of the given list.

  procedure Swap_Tail (Obj : in out Single_List;
                       With_List : in out Single_List);
  -- The given list must represent the head of a list, which may be
  -- null. Set the tail of the list (which may be null) to denote the given
  -- list (which may be null), and set the given list to the original tail
  -- of the list. If it is not null, the predecessor of the new tail of the
  -- list is set to be the head of the list. If it is not null, the
  -- predecessor of the new head of the given list is set to be null.

  procedure Tail (Obj : in out Single_List);
  -- The list must not be null. Set the list to now denote its tail (which
  -- may be null), and reclaim the storage associated with any unreachable
  -- items.

  procedure Set_Head (Obj : in out Single_List; Elem : Item);
  -- Set the item at the head of the list.

  procedure Set_Item (Obj : in out Single_List;
                      Elem : Item;
                      At_Loc : Positive);
  -- Set the item at the given index.

  function Length (Obj : Single_List) return Natural;
  -- Return the number of items in the list.

  function Is_Null (Obj : Single_List) return Boolean;
  -- Return True if and only there are no items in the list.

  function Is_Shared (Obj : Single_List) return Boolean;
  -- Return True if and only if the list has an alias.

  function Head (Obj : Single_List) return Item;
  -- Return a copy of the item at the head of the list.

  -- XXX need accessor generic

  function Foot (Obj : Single_List) return Item;
  -- Return a copy of the item at the end of the list.

  -- XXX need accessor generic

  function Item_At (Obj : Single_List; Index : Positive) return Item;
  -- Return a copy of the item at the given index.

  function New_Iterator (For_The_List : Single_List) return Iterator;
  -- Return a reset Iterator bound to the specific List.

private

  function Item_At (Obj : Single_List; Index : Positive) return Item_Ptr;
  function Cardinality (Obj : Single_List) return Natural;

  package Single_Nodes
  is new BC.Support.Nodes (Item, Storage_Manager, Storage);

  type Single_List is new Container with record
    Rep : Single_Nodes.Single_Node_Ref;
  end record;

  procedure Initialize (Obj : in out Single_List);
  procedure Adjust (Obj : in out Single_List);
  procedure Finalize (Obj : in out Single_List);

  type Single_List_Iterator (L : access Single_List'Class)
  is new Actual_Iterator (L) with record
    Index : Natural;
  end record;

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Initialize (It : in out Single_List_Iterator);

  procedure Reset (It : in out Single_List_Iterator);

  procedure Next (It : in out Single_List_Iterator);

  function Is_Done (It : Single_List_Iterator) return Boolean;

  function Current_Item (It : Single_List_Iterator) return Item;

  function Current_Item (It : Single_List_Iterator) return Item_Ptr;

end BC.Containers.Lists.Single;
