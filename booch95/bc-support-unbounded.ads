-- Copyright (C) 1994-1998 Grady Booch, David Weller, Pat Rogers and
-- Simon Wright.
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
  type Item is private;
  type Item_Ptr is access all Item;
  type Storage_Manager(<>)
  is new System.Storage_Pools.Root_Storage_Pool with private;
  Storage : in out Storage_Manager;
package BC.Support.Unbounded is

  type Unb_Node is private;

  type Unb_Node_Ref is access Unb_Node;

  function Create (From : Unb_Node) return Unb_Node;

  function "=" (Left, Right : Unb_Node) return Boolean;

  procedure Clear (Obj : in out Unb_Node);
  procedure Insert (Obj : in out Unb_Node; Elem : Item);
  procedure Insert (Obj : in out Unb_Node; Elem : Item; Before : Natural);
  procedure Append (Obj : in out Unb_Node; Elem : Item);
  procedure Append (Obj : in out Unb_Node; Elem : Item; After : Natural);
  procedure Remove (Obj : in out Unb_Node; From : Natural);
  procedure Replace (Obj : in out Unb_Node; Index : Positive; Elem : Item);
  function Length (Obj : Unb_Node) return Natural;
  function First (Obj : Unb_Node) return Item;
  function First (Obj : access Unb_Node) return Item_Ptr;
  function Last (Obj : Unb_Node) return Item;
  function Last (Obj : access Unb_Node) return Item_Ptr;
  function Item_At (Obj : access Unb_Node; Index : Positive) return Item;
  function Item_At (Obj : access Unb_Node; Index : Positive) return Item_Ptr;
  function Location (Obj : access Unb_Node; Elem : Item; Start : Positive := 1)
                     return Natural;

  procedure Free (Obj : in out Unb_Node_Ref);
  -- Dispose of the Node referred to, having first Cleared it

private

  package Nodes is new Bc.Support.Nodes (Item, Storage_Manager, Storage);

  type Unb_Node is record
    Rep : Nodes.Node_Ref;
    Last : Nodes.Node_Ref;
    Size : Natural := 0;
    Cache : Nodes.Node_Ref;
    Cache_Index : Natural := 0;
  end record;

end BC.Support.Unbounded;
