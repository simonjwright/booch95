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

generic
package Bc.Containers.Lists.Single is

  -- Singly-linked list

  type Single_List is new Container with private;

  function "=" (L, R : Single_List) return Boolean;

  procedure Clear (Obj : in out Single_List);
  procedure Insert (Obj : in out Single_List; Elem : Item);
  procedure Insert (Obj : in out Single_List; From_List : in Single_List);
  procedure Insert (Obj : in out Single_List; Elem : Item; Before : Positive);
  procedure Insert (Obj : in out Single_List;
                    From_List: in out Single_List;
                    Before : Positive);
  procedure Append (Obj : in out Single_List; Elem : Item);
  procedure Append (Obj : in out Single_List; From_List : in Single_List);
  procedure Append (Obj : in out Single_List; Elem : Item; After : Positive);
  procedure Append (Obj : in out Single_List;
                    From_List : in Single_List;
                    After : Positive);
  procedure Remove (Obj : in out Single_List; From : Positive);
  procedure Purge (Obj : in out Single_List; From : Positive);
  procedure Purge (Obj : in out Single_List; From : Positive; Count : Positive);
  procedure Preserve (Obj : in out Single_List; From : Positive);
  procedure Preserve (Obj : in out Single_List;
                      From : Positive;
                      Count : Positive);
  procedure Share (Obj : in out Single_List;
                   With_List: Single_List;
                   Starting_At : Positive);
  procedure Share_Head (Obj : in out Single_List; With_List : in Single_List);
  procedure Share_Foot (Obj : in out Single_List; With_List : in Single_List);
  procedure Swap_Tail (Obj : in out Single_List;
                       With_List : in out Single_List);
  procedure Tail (Obj : in out Single_List);
  procedure Set_Head (Obj : in out Single_List; Elem : Item);
  procedure Set_Item (Obj : in out Single_List;
                      Elem : Item;
                      At_Loc : Positive);

  function Length (Obj : Single_List) return Natural;
  function Is_Null (Obj : Single_List) return Boolean;
  function Is_Shared (Obj : Single_List) return Boolean;
  function Head (Obj : Single_List) return Item;
  function Head (Obj : Single_List) return Item_Ptr;
  function Foot (Obj : Single_List) return Item;
  function Foot (Obj : Single_List) return Item_Ptr;
  function Item_At (Obj : Single_List; Index : Positive) return Item;

private

  function Item_At (Obj : Single_List; Index : Natural) return Item_Ptr;
  function Cardinality (Obj : Single_List) return Integer;

  package Single_Nodes is new Bc.Support.Nodes (Item);

  type Single_List is new Container with record
    Rep : Single_Nodes.Single_Node_Ref;
  end record;

  procedure Initialize (Obj : in out Single_List);
  procedure Adjust (Obj : in out Single_List);
  procedure Finalize (Obj : in out Single_List);

end Bc.Containers.Lists.Single;
