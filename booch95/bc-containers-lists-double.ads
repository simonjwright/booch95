--  The C++ Booch Components (Version 2.3)
--  (C) Copyright 1990-1994 Grady Booch. All Rights Reserved.
--
--  BCDList.h
--
--  This file contains the declaration of the doubly-linked list.

with Bc.Support.Nodes;
generic
package Bc.Containers.Lists.Double is

-- Doubly-linked list

   type Double_List is new Container with private;

   function Create(Obj : Double_List) return Double_List;

   function "="(L, R : Double_List) return Boolean;

   procedure Clear(Obj : in out Double_List);
   procedure Insert(Obj : in out Double_List; Elem : Item);
   procedure Insert(Obj : in out Double_List; From_List : in Double_List);
   procedure Insert(Obj : in out Double_List; Elem : Item; Before : Positive);
   procedure Insert(Obj : in out Double_List; From_List: in out Double_List;
					      Before : Positive);
   procedure Append(Obj : in out Double_List; Elem : Item);
   procedure Append(Obj : in out Double_List; From_List : in Double_List);
   procedure Append(Obj : in out Double_List; Elem : Item; After : Natural);
   procedure Append(Obj : in out Double_List; From_List : in Double_List;
					      After : Natural);
   procedure Remove(Obj : in out Double_List; From : Natural);
   procedure Purge (Obj : in out Double_LIst; From : Natural);
   procedure Purge (Obj : in out Double_List; From : Natural; Count: Positive);
   procedure Preserve(Obj : in out Double_List; From : Natural);
   procedure Preserve(Obj : in out Double_List; From: Natural; Count:Positive);
   procedure Share (Obj : in out Double_List; With_List: Double_List;
					      Starting_At : Positive);
   procedure Share_Head(Obj : in out Double_List; With_List : in Double_List);
   procedure Share_Foot(Obj : in out Double_List; With_List : in Double_List);
   procedure Swap_Tail (Obj : in out Double_List;
			With_List : in out Double_List);
   procedure Tail(Obj : in out Double_List);
   procedure Predecessor(Obj : in out Double_List);
   procedure Set_Head(Obj : in out Double_List; Elem : Item);
   procedure Set_Item(Obj : in out Double_List; Elem : Item; At_Loc: Positive);

   function Length(Obj : Double_List) return Natural;
   function Is_Null(Obj : Double_List) return Boolean;
   function Is_Shared(Obj : Double_List) return Boolean;
   function Is_Head(Obj : Double_List) return Boolean;
   function Head(Obj : Double_List) return Item;
   function Head(Obj : Double_List) return Item_Ptr;
   function Foot(Obj : Double_List) return Item;
   function Foot(Obj : Double_List) return Item_Ptr;
   function Item_At(Obj : Double_List; Loc : Natural) return Item;

private
   function Item_At(Obj : Double_List; Loc : Natural) return Item_Ptr;

   package Double_Nodes is new Bc.Support.Nodes(Item);

   type Double_List is new Container with record
      Rep : Double_Nodes.Double_Node_Ref;
   end record;

   procedure Initialize( Obj : in out Double_List);
   procedure Adjust(Obj : in out Double_List);
   procedure Finalize(Obj : in out Double_List);

end Bc.Containers.Lists.Double;
