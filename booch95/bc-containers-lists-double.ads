-- Copyright (C) 1994-2000 Grady Booch, David Weller and Simon Wright.
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
package BC.Containers.Lists.Double is

  pragma Elaborate_Body;

  -- Doubly-linked list

  type Double_List is new Container with private;

  function "=" (L, R : Double_List) return Boolean;
  -- Return True if and only if both lists are null or structurally share
  -- the same list.

  procedure Clear (L : in out Double_List);
  -- If the list is not null, destroy this alias to the list, make the list
  -- null, and reclaim the storage associated with any unreachable items.

  procedure Insert (L : in out Double_List; Elem : Item);
  -- Add the item to the head of the list.

  procedure Insert (L : in out Double_List; From_List : in Double_List);
  -- Add the given list to the head of the list.

  procedure Insert (L : in out Double_List; Elem : Item; Before : Positive);
  -- Add the item before the given index item in the list; if before is 1,
  -- the item is added to the head of the list.

  procedure Insert (L : in out Double_List;
                    From_List: in out Double_List;
                    Before : Positive);
  -- Add the list before the given index item in the list; if before is 1,
  -- the list is added to the head of the list.

  procedure Append (L : in out Double_List; Elem : Item);
  -- Add the item at the end of the list.

  procedure Append (L : in out Double_List; From_List : in Double_List);
  -- Add the given list at the end of the list.

  procedure Append (L : in out Double_List; Elem : Item; After : Positive);
  -- Add the item after the given index item in the list.

  procedure Append (L : in out Double_List;
                    From_List : in Double_List;
                    After : Positive);
  -- Add the list after the given index item in the list.

  procedure Remove (L : in out Double_List; From : Positive);
  -- Remove the item at the given index in the list.

  procedure Purge (L : in out Double_LIst; From : Positive);
  -- Remove all the items in the list starting at the given index,
  -- inclusive.

  procedure Purge (L : in out Double_List;
                   From : Positive;
                   Count : Positive);
  -- Remove all the items in the list starting at the given index,
  -- inclusive, for a total of count items.

  procedure Preserve (L : in out Double_List; From : Positive);
  -- Remove all the items in the list except those starting at the given
  -- index, inclusive.

  procedure Preserve (L : in out Double_List;
                      From : Positive;
                      Count : Positive);
  -- Remove all the items in the list except those starting at the given
  -- index, inclusive, for a total of count items.

  procedure Share (L : in out Double_List;
                   With_List : Double_List;
                   Starting_At : Positive);
  -- Clear the list, then, if the given list is not null, set the list to
  -- structurally share with the head of the given list, starting at the
  -- given index.

  procedure Share_Head (L : in out Double_List; With_List : in Double_List);
  -- Clear the list, then, if the given list is not null, set the list to
  -- structurally share with the head of the given list.

  procedure Share_Foot (L : in out Double_List; With_List : in Double_List);
  -- Clear the list, then, if the given list is not null, set the list to
  -- structurally share with the end of the given list.

  procedure Swap_Tail (L : in out Double_List;
                       With_List : in out Double_List);
  -- The given list must represent the head of a list, which may be
  -- null. Set the tail of the list (which may be null) to denote the given
  -- list (which may be null), and set the given list to the original tail
  -- of the list. If it is not null, the predecessor of the new tail of the
  -- list is set to be the head of the list. If it is not null, the
  -- predecessor of the new head of the given list is set to be null.

  procedure Tail (L : in out Double_List);
  -- The list must not be null. Set the list to now denote its tail (which
  -- may be null), and reclaim the storage associated with any unreachable
  -- items.

  procedure Predecessor (L : in out Double_List);
  -- Set the list to now denote its predecessor (if any)

  procedure Set_Head (L : in out Double_List; Elem : Item);
  -- Set the item at the head of the list.

  procedure Set_Item (L : in out Double_List;
                      Elem : Item;
                      At_Loc: Positive);
  -- Set the item at the given index.

  function Length (L : Double_List) return Natural;
  -- Return the number of items in the list.

  function Is_Null (L : Double_List) return Boolean;
  -- Return True if and only there are no items in the list.

  function Is_Shared (L : Double_List) return Boolean;
  -- Return True if and only if the list has an alias.

  function Is_Head (L : Double_List) return Boolean;
  -- Return True if and only if the list is at the head.

  function Head (L : Double_List) return Item;
  -- Return a copy of the item at the head of the list.

  generic
    with procedure Process (Elem : in out Item);
  procedure Process_Head (L : in out Double_List);
  -- Access the item at the head of the list.

  function Foot (L : Double_List) return Item;
  -- Return a copy of the item at the end of the list.

  generic
    with procedure Process (Elem : in out Item);
  procedure Process_Foot (L : in out Double_List);
  -- Access the item at the end of the list.

  function Item_At (L : Double_List; Index : Positive) return Item;
  -- Return a copy of the item at the given index.

  function New_Iterator (For_The_List : Double_List) return Iterator'Class;
  -- Return a reset Iterator bound to the specific List.

private

  function Item_At (L : Double_List; Index : Positive) return Item_Ptr;

  package Double_Nodes
  is new BC.Support.Nodes (Item, Storage_Manager, Storage);

  type Double_List is new Container with record
    Rep : Double_Nodes.Double_Node_Ref;
  end record;

  procedure Initialize (L : in out Double_List);
  procedure Adjust (L : in out Double_List);
  procedure Finalize (L : in out Double_List);

  type Double_List_Iterator is new Iterator with record
    Index : Double_Nodes.Double_Node_Ref;
  end record;

  -- Overriding primitive supbrograms of the concrete actual Iterator.

  procedure Reset (It : in out Double_List_Iterator);

  procedure Next (It : in out Double_List_Iterator);

  function Is_Done (It : Double_List_Iterator) return Boolean;

  function Current_Item (It : Double_List_Iterator) return Item;

  function Current_Item_Ptr (It : Double_List_Iterator) return Item_Ptr;

  procedure Delete_Item_At (It : Double_List_Iterator);

end BC.Containers.Lists.Double;
