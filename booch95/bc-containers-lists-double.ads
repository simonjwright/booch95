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

with Bc.Support.Nodes;
with System.Storage_Pools;

generic
  type Storage_Manager (<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package Bc.Containers.Lists.Double is

  -- Doubly-linked list

  type Double_List is new Container with private;

  function "=" (L, R : Double_List) return Boolean;

  procedure Clear (Obj : in out Double_List);
  procedure Insert (Obj : in out Double_List; Elem : Item);
  procedure Insert (Obj : in out Double_List; From_List : in Double_List);
  procedure Insert (Obj : in out Double_List; Elem : Item; Before : Positive);
  procedure Insert (Obj : in out Double_List;
                    From_List: in out Double_List;
                    Before : Positive);
  procedure Append (Obj : in out Double_List; Elem : Item);
  procedure Append (Obj : in out Double_List; From_List : in Double_List);
  procedure Append (Obj : in out Double_List; Elem : Item; After : Positive);
  procedure Append (Obj : in out Double_List;
                    From_List : in Double_List;
                    After : Positive);
  procedure Remove (Obj : in out Double_List; From : Positive);
  procedure Purge (Obj : in out Double_LIst; From : Positive);
  procedure Purge (Obj : in out Double_List;
                   From : Positive;
                   Count : Positive);
  procedure Preserve (Obj : in out Double_List; From : Positive);
  procedure Preserve (Obj : in out Double_List;
                      From : Positive;
                      Count : Positive);
  procedure Share (Obj : in out Double_List;
                   With_List : Double_List;
                   Starting_At : Positive);
  procedure Share_Head (Obj : in out Double_List; With_List : in Double_List);
  procedure Share_Foot (Obj : in out Double_List; With_List : in Double_List);
  procedure Swap_Tail (Obj : in out Double_List;
                       With_List : in out Double_List);
  procedure Tail (Obj : in out Double_List);
  procedure Predecessor (Obj : in out Double_List);
  procedure Set_Head (Obj : in out Double_List; Elem : Item);
  procedure Set_Item (Obj : in out Double_List;
                      Elem : Item;
                      At_Loc: Positive);

  function Length (Obj : Double_List) return Natural;
  function Is_Null (Obj : Double_List) return Boolean;
  function Is_Shared (Obj : Double_List) return Boolean;
  function Is_Head (Obj : Double_List) return Boolean;
  function Head (Obj : Double_List) return Item;
  function Head (Obj : Double_List) return Item_Ptr;
  function Foot (Obj : Double_List) return Item;
  function Foot (Obj : Double_List) return Item_Ptr;
  function Item_At (Obj : Double_List; Index : Positive) return Item;

private

  function Item_At (Obj : Double_List; Index : Natural) return Item_Ptr;
  function Cardinality (Obj : Double_List) return Integer;

  package Double_Nodes
  is new Bc.Support.Nodes (Item, Storage_Manager, Storage);

  type Double_List is new Container with record
    Rep : Double_Nodes.Double_Node_Ref;
  end record;

  procedure Initialize (Obj : in out Double_List);
  procedure Adjust (Obj : in out Double_List);
  procedure Finalize (Obj : in out Double_List);

end Bc.Containers.Lists.Double;
