--  The C++ Booch Components (Version 2.3)
--  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--
--  BCList.h
--
--  This file contains the declaration of the singly-linked list.

with Bc.Support.Nodes;
generic
package Bc.Containers.Lists.Single is

-- Singly-linked list

   type Single_List is new Container with private;

   function Create(Obj : Single_List) return Single_List;

   function "="(L, R : Single_List) return Boolean;

   procedure Clear (Obj : in out Single_List);
   procedure Insert(Obj : in out Single_List; Elem : Item);
   procedure Insert(Obj : in out Single_List; From_List : in Single_List);
   procedure Insert(Obj : in out Single_List; Elem : Item; Before : Positive);
   procedure Insert(Obj : in out Single_List; From_List: in out Single_List;
					      Before : Positive);
   procedure Append(Obj : in out Single_List; Elem : Item);
   procedure Append(Obj : in out Single_List; From_List : in Single_List);
   procedure Append(Obj : in out Single_List; Elem : Item; After : Natural);
   procedure Append(Obj : in out Single_List; From_List : in Single_List;
					      After : Natural);
   procedure Remove(Obj : in out Single_List; From : Natural);
   procedure Purge (Obj : in out Single_LIst; From : Natural);
   procedure Purge (Obj : in out Single_List; From : Natural; Count: Positive);
   procedure Preserve(Obj : in out Single_List; From : Natural);
   procedure Preserve(Obj : in out Single_List; From: Natural; Count:Positive);
   procedure Share (Obj : in out Single_List; With_List: Single_List;
					      Starting_At : Positive);
   procedure Share_Head(Obj : in out Single_List; With_List : in Single_List);
   procedure Share_Foot(Obj : in out Single_List; With_List : in Single_List);
   procedure Swap_Tail (Obj : in out Single_List;
			With_List : in out Single_List);
   procedure Tail(Obj : in out Single_List);
   procedure Set_Head(Obj : in out Single_List; Elem : Item);
   procedure Set_Item(Obj : in out Single_List; Elem : Item; At_Loc: Positive);

   function Length(Obj : Single_List) return Natural;
   function Is_Null(Obj : Single_List) return Boolean;
   function Is_Shared(Obj : Single_List) return Boolean;
   function Head(Obj : Single_List) return Item;
   function Head(Obj : Single_List) return Item_Ptr;
   function Foot(Obj : Single_List) return Item;
   function Foot(Obj : Single_List) return Item_Ptr;
   function Item_At(Obj : Single_List; Loc : Natural) return Item;

private
   function Item_At(Obj : Single_List; Loc : Natural) return Item_Ptr;

   package Single_Nodes is new Bc.Support.Nodes(Item);

   type Single_List is new Container with record
      Rep : Single_Nodes.Single_Node_Ref;
   end record;

   procedure Initialize( Obj : in out Single_List);
   procedure Adjust(Obj : in out Single_List);
   procedure Finalize(Obj : in out Single_List);

end Bc.Containers.Lists.Single;





















